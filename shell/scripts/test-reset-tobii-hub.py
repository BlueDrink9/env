#!/usr/bin/env python3 -m pytest -v

import pytest
import importlib.util
from pathlib import Path
from unittest.mock import patch, MagicMock

# ==========================================
# SETUP & DYNAMIC SCRIPT LOADING
# ==========================================

script_path = Path(__file__).parent / "reset-tobii-hub.py"
if not script_path.exists():
    pytest.skip(f"Script not found at {script_path}. Please place this test next to it.", allow_module_level=True)

spec = importlib.util.spec_from_file_location("reset_hub", script_path)
reset_hub = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reset_hub)

# Alias the dataclass for easier typing in tests
UsbDevice = reset_hub.UsbDevice


# ==========================================
# MOCK DATA
# ==========================================

MOCK_LSUSB = """Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 001 Device 002: ID 05e3:0610 Genesys Logic, Inc. Hub
Bus 001 Device 003: ID 0424:2514 Microchip Technology, Inc. USB 2.0 Hub
Bus 001 Device 011: ID 2104:0313 Tobii Technology AB EyeChip"""

MOCK_LSUSB_T = """/:  Bus 001.Port 001: Dev 001, Class=root_hub, Driver=xhci_hcd/2p, 480M
    |__ Port 001: Dev 002, If 0, Class=Hub, Driver=hub/4p, 480M
        |__ Port 001: Dev 003, If 0, Class=Hub, Driver=hub/4p, 480M
        |__ Port 002: Dev 011, If 0, Class=Vendor Specific Class, Driver=usbfs, 480M
"""


# ==========================================
# 1. LOW-LEVEL PARSING UTILITIES
# ==========================================

def test_extract_bus_number():
    assert reset_hub.extract_bus_number("/:  Bus 003.Port 001: Dev 001") == "003"
    assert reset_hub.extract_bus_number("    |__ Port 001: Dev 115") is None

def test_extract_device_number():
    assert reset_hub.extract_device_number("    |__ Port 001: Dev 115, If 0") == "115"
    assert reset_hub.extract_device_number("Just some text without a device") is None

def test_get_indent_level():
    assert reset_hub.get_indent_level("    |__ Port 001") == 4
    assert reset_hub.get_indent_level("        |__ Port 001") == 8
    assert reset_hub.get_indent_level("/:  Bus 001") == 0

def test_assign_parent():
    root = UsbDevice("1", "1", "", "", "Root", True, 0, "1/1", "")
    hub = UsbDevice("1", "2", "", "", "Hub", True, 4, "1/2", "")
    device = UsbDevice("1", "3", "", "", "Dev", False, 8, "1/3", "")

    parent_stack = [(0, root), (4, hub)]

    reset_hub.assign_parent(device, parent_stack)
    assert device.parent == hub

    # Test sibling behavior (indent drops back down)
    sibling_hub = UsbDevice("1", "4", "", "", "Sibling Hub", True, 4, "1/4", "")
    reset_hub.assign_parent(sibling_hub, parent_stack)

    # assign_parent modifies the stack by popping deeper items
    assert sibling_hub.parent == root


# ==========================================
# 2. MID-LEVEL PARSING
# ==========================================

def test_parse_lsusb_lookup():
    lookup = reset_hub.parse_lsusb_lookup(MOCK_LSUSB)

    assert "001/011" in lookup
    assert lookup["001/011"]["vendor_id"] == "2104"
    assert lookup["001/011"]["product_id"] == "0313"
    assert "Tobii" in lookup["001/011"]["name"]

def test_parse_flat_tree_sequence():
    sequence = reset_hub.parse_flat_tree_sequence(MOCK_LSUSB_T)

    assert len(sequence) == 4
    assert sequence[0]["dev_num"] == "001"
    assert sequence[0]["is_hub"] is True
    assert sequence[3]["dev_num"] == "011"
    assert sequence[3]["bus"] == "001"
    assert sequence[3]["is_hub"] is False

def test_build_device_tree_missing_lookup():
    """Verify that a device found in the tree gracefully handles missing lsusb lookup data."""
    sequence = [{"bus": "002", "dev_num": "099", "indent_level": 4, "is_hub": False, "line": "Dev 099"}]
    empty_lookup = {}

    devices = reset_hub.build_device_tree(sequence, empty_lookup)

    assert len(devices) == 1
    assert devices[0].name == "Unknown Device"
    assert devices[0].vendor_id == ""


def test_build_device_tree_late_deduplication():
    """
    Verify the builder deduplicates physical devices and safely bypasses
    the parent stack logic so the hierarchy remains uncorrupted.
    """
    # 1. Provide a raw sequence with duplicates (simulating the dumb parser output)
    sequence_with_duplicates = [
        {"bus": "003", "dev_num": "001", "indent_level": 0, "is_hub": True, "line": "Dev 001"},
        {"bus": "003", "dev_num": "021", "indent_level": 4, "is_hub": False, "line": "Dev 021, If 0"},
        {"bus": "003", "dev_num": "021", "indent_level": 4, "is_hub": False, "line": "Dev 021, If 1"},
        {"bus": "003", "dev_num": "021", "indent_level": 4, "is_hub": False, "line": "Dev 021, If 2"},
    ]

    # Minimal lookup data
    mock_lookup = {
        "003/001": {"name": "Root Hub"},
        "003/021": {"name": "Tobii EyeChip"}
    }

    devices = reset_hub.build_device_tree(sequence_with_duplicates, mock_lookup)

    # 2. PROVE DEDUPLICATION: We expect exactly 2 UsbDevice objects created
    assert len(devices) == 2, f"Expected exactly 2 physical devices, but got {len(devices)}"

    root_hub = devices[0]
    eyechip = devices[1]

    assert root_hub.device_num == "001"
    assert eyechip.device_num == "021"

    # 3. PROVE STACK INTEGRITY: Verify the parent stack wasn't corrupted
    # If the stack logic processed 'If 1' and 'If 2', it might have popped 'If 0' off
    # the stack incorrectly. This ensures the eyechip correctly points to the root hub.
    assert eyechip.parent == root_hub


# ==========================================
# 3. HIGH-LEVEL LOGIC (OPERATING ON OBJECTS)
# ==========================================

@pytest.fixture
def parsed_devices():
    """Fixture to provide a fully parsed tree for the logic tests."""
    return reset_hub.parse_usb_devices(MOCK_LSUSB_T, MOCK_LSUSB)

def test_find_eyechip_device(parsed_devices):
    device = reset_hub.find_eyechip_device(parsed_devices)
    assert device is not None
    assert device.device_num == "011"

def test_find_parent_hubs(parsed_devices):
    eyechip = reset_hub.find_eyechip_device(parsed_devices)
    parents = reset_hub.find_parent_hubs(eyechip)

    assert len(parents) == 2
    # Ensure traversal is bottom-up (immediate parent first)
    assert parents[0].device_num == "002"
    assert parents[1].device_num == "001"
    # Ensure sibling Dev 003 is ignored
    assert "003" not in [p.device_num for p in parents]

def test_find_root_hub(parsed_devices):
    root = reset_hub.find_root_hub(parsed_devices)
    assert root is not None
    assert root.device_num == "001"
    assert root.indent_level == 0

def test_select_hub_to_reset(parsed_devices):
    # Mocking a list of parents
    parents = [
        UsbDevice("1", "2", "", "", "Grandparent Hub", True, 4, "1/2", ""),
        UsbDevice("1", "3", "", "", "Immediate Parent Hub", True, 8, "1/3", "")
    ]

    selected = reset_hub.select_hub_to_reset(parents, parsed_devices)

    # Since the sibling bug is fixed, we now expect it to grab the immediate parent
    # (the one with the highest indent level, which is index -1 after sorting)
    assert selected is not None
    assert selected.name == "Immediate Parent Hub"
    assert selected.device_num == "3"

def test_select_hub_to_reset_fallback(parsed_devices):
    # Pass empty hierarchy to trigger fallback
    selected = reset_hub.select_hub_to_reset([], parsed_devices)

    # Should fallback to root hub (Dev 001)
    assert selected is not None
    assert selected.device_num == "001"


# ==========================================
# 4. FULL INTEGRATION TESTS
# ==========================================

@patch.object(reset_hub.subprocess, "run")
@patch.object(reset_hub, "run_lsusb_commands")
def test_find_eyechip_and_reset_hub_success(mock_run_lsusb, mock_subprocess_run):
    """Test the entire flow from text to successful usbreset command."""
    # Mock lsusb commands
    mock_run_lsusb.return_value = (MOCK_LSUSB_T, MOCK_LSUSB)

    # Mock usbreset command success
    mock_subprocess_run.return_value = MagicMock(stdout="Reset successful", returncode=0)

    success = reset_hub.find_eyechip_and_reset_hub()

    assert success is True
    # Verify subprocess.run was called exactly once by reset_usb_hub
    mock_subprocess_run.assert_called_once()

    # Verify it called usbreset on the IMMEDIATE parent hub (001/003)
    called_args = mock_subprocess_run.call_args[0][0]
    assert called_args == ["usbreset", "001/002"]

@patch.object(reset_hub, "run_lsusb_commands")
def test_find_eyechip_and_reset_hub_no_device(mock_run_lsusb):
    """Test full flow when the EyeChip is completely missing."""
    mock_run_lsusb.return_value = (
        "/:  Bus 001.Port 001: Dev 001, Class=root_hub",
        "Bus 001 Device 001: ID 1d6b:0002 Linux Foundation"
    )

    success = reset_hub.find_eyechip_and_reset_hub()
    assert success is False

# ==========================================
# REAL WORLD INTEGRATION MOCKS
# ==========================================

# Minimal standard lsusb to match the tree branch we care about
REAL_MOCK_LSUSB = """Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 002 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 003 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 003 Device 115: ID 03f0:0c82 HP, Inc USB2816 Smart Hub
Bus 003 Device 015: ID 05e3:0610 Genesys Logic, Inc. Hub
Bus 003 Device 021: ID 2104:0313 Tobii Technology AB EyeChip
Bus 004 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub"""

# A more complete full tree from one test machine with lots of hubs, and multiple eyechip interfaces
REAL_MOCK_LSUSB_T = """/:  Bus 001.Port 001: Dev 001, Class=root_hub, Driver=xhci_hcd/1p, 480M
/:  Bus 002.Port 001: Dev 001, Class=root_hub, Driver=xhci_hcd/4p, 20000M/x2
    |__ Port 001: Dev 010, If 0, Class=Hub, Driver=hub/4p, 5000M
        |__ Port 003: Dev 012, If 0, Class=Hub, Driver=hub/4p, 5000M
/:  Bus 003.Port 001: Dev 001, Class=root_hub, Driver=xhci_hcd/12p, 480M
    |__ Port 001: Dev 115, If 0, Class=Hub, Driver=hub/5p, 480M
        |__ Port 002: Dev 013, If 0, Class=Human Interface Device, Driver=usbhid, 12M
        |__ Port 003: Dev 015, If 0, Class=Hub, Driver=hub/4p, 480M
            |__ Port 004: Dev 021, If 0, Class=Vendor Specific Class, Driver=usbfs, 480M
            |__ Port 004: Dev 021, If 1, Class=Video, Driver=[none], 480M
            |__ Port 004: Dev 021, If 2, Class=Video, Driver=[none], 480M
        |__ Port 004: Dev 017, If 0, Class=Human Interface Device, Driver=usbhid, 12M
        |__ Port 004: Dev 017, If 1, Class=Human Interface Device, Driver=usbhid, 12M
        |__ Port 004: Dev 017, If 2, Class=Human Interface Device, Driver=usbhid, 12M
/:  Bus 004.Port 001: Dev 001, Class=root_hub, Driver=xhci_hcd/2p, 20000M/x2"""


def test_real_world_hierarchy_and_deduplication():
    """Verify that multiple interfaces don't create duplicate objects and hierarchy is perfect."""
    # 1. Parse the massive real-world tree
    parsed_devices = reset_hub.parse_usb_devices(REAL_MOCK_LSUSB_T, REAL_MOCK_LSUSB)

    # 2. PROVE DEDUPLICATION: Count occurrences of Dev 021 (Tobii)
    # The string has 3 interfaces for Dev 021, but our list should only have 1 device object
    dev_021_objects = [d for d in parsed_devices if d.bus == "003" and d.device_num == "021"]
    assert len(dev_021_objects) == 1, "Deduplication failed! Multiple interfaces created multiple objects."

    # 3. PROVE HIERARCHY: Trace the lineage of Dev 021
    eyechip = dev_021_objects[0]
    parents = reset_hub.find_parent_hubs(eyechip)

    # We expect exactly 3 parent hubs based on the real string's indentation
    assert len(parents) == 3, f"Expected 3 parents, found {len(parents)}"

    # Immediate parent (Level 2)
    assert parents[0].device_num == "015"
    assert "Genesys" in parents[0].name

    # Grandparent (Level 1)
    assert parents[1].device_num == "115"
    assert "HP" in parents[1].name

    # Great-grandparent / Root (Level 0)
    assert parents[2].device_num == "001"
    assert "root" in parents[2].name.lower()

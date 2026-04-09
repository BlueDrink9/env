#!/usr/bin/env python3
# requirements: python3 usbutils gnugrep util-linux

import subprocess
import re
import sys
from dataclasses import dataclass
from typing import Optional


@dataclass
class UsbDevice:
    """Represents a structured USB device with parent/child hierarchy."""
    bus: str
    device_num: str
    vendor_id: str
    product_id: str
    name: str
    is_hub: bool
    indent_level: int
    bus_device: str
    full_line: str
    parent: Optional['UsbDevice'] = None


def find_eyechip_and_reset_hub() -> bool:
    """
    Find eyechip/Tobii device and reset a suitable parent hub.
    Returns True on success, False on failure.
    """
    try:
        tree_output, lsusb_output = run_lsusb_commands()
    except RuntimeError as e:
        print(f"ERROR: {e}")
        return False

    # Centralized parsing
    devices = parse_usb_devices(tree_output, lsusb_output)

    eyechip_device = find_eyechip_device(devices)
    if not eyechip_device:
        print("ERROR: No eyechip/Tobii device found in USB devices")
        return False

    hub_hierarchy = find_parent_hubs(eyechip_device)

    # Pass both the hierarchy and the full device list for fallback checking
    selected_hub = select_hub_to_reset(hub_hierarchy, devices)

    if not selected_hub:
        print("ERROR: Could not determine suitable parent hub or root hub to reset")
        return False

    return reset_usb_hub(selected_hub)


def run_lsusb_commands() -> tuple[str, str]:
    """Get USB device tree and listing output."""
    try:
        tree_result = subprocess.run(
            ["lsusb", "-t"], capture_output=True, text=True, check=True
        )
        lsusb_result = subprocess.run(
            ["lsusb"], capture_output=True, text=True, check=True
        )
        return tree_result.stdout, lsusb_result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to run lsusb: {e}")


def parse_usb_devices(tree_output: str, lsusb_output: str) -> list[UsbDevice]:
    """
    Parse both lsusb outputs into a structured list of UsbDevice objects,
    establishing parent/child hierarchy automatically.
    """
    lsusb_lookup = parse_lsusb_lookup(lsusb_output)
    return build_device_tree(tree_output, lsusb_lookup)


def find_eyechip_device(devices: list[UsbDevice]) -> UsbDevice | None:
    """Find eyechip/Tobii device in parsed objects."""
    for device in devices:
        if "tobii" in device.name.lower() or "eyechip" in device.name.lower():
            print(f"Found eyechip device: {device.full_line}")
            print(f"Found eyechip in USB tree at indent level: {device.indent_level}")
            return device
    return None


def find_parent_hubs(eyechip_device: UsbDevice) -> list[UsbDevice]:
    """Find all direct parent hubs of the eyechip device by traversing the object tree."""
    hub_hierarchy: list[UsbDevice] = []

    current_parent = eyechip_device.parent

    # Traverse straight up the established parent chain
    while current_parent:
        if current_parent.is_hub:
            print(f"Found parent hub: {current_parent.name} (indent: {current_parent.indent_level})")
            hub_hierarchy.append(current_parent)

        current_parent = current_parent.parent

    return hub_hierarchy


def find_root_hub(devices: list[UsbDevice]) -> UsbDevice | None:
    """Find the root hub as a fallback option."""
    for device in devices:
        if device.is_hub and device.indent_level == 0:
            print(f"Found root hub: {device.name}")
            return device
    return None


def select_hub_to_reset(hub_hierarchy: list[UsbDevice], devices: list[UsbDevice]) -> UsbDevice | None:
    """Choose the most appropriate hub to reset, falling back to the root hub if needed."""
    if not hub_hierarchy:
        print("No parent hubs found for eyechip device, trying to find root hub as fallback...")
        root_hub = find_root_hub(devices)
        if root_hub:
            print("Using root hub as fallback")
        return root_hub

    # Sort by indent level (lower indent = higher in hierarchy)
    hub_hierarchy.sort(key=lambda x: x.indent_level)

    print(f"Found {len(hub_hierarchy)} parent hubs in hierarchy:")
    for i, hub in enumerate(hub_hierarchy):
        print(f"  Level {i}: {hub.name}")

    if len(hub_hierarchy) >= 1:
        selected = hub_hierarchy[-1]
        print(f"Using hub: {selected.name}")
        return selected

    # Final safety net (though practically unreachable if len >= 1 is checked above)
    print("Could not select a hub from hierarchy, trying root hub as fallback...")
    root_hub = find_root_hub(devices)
    if root_hub:
        print("Using root hub as fallback")
    return root_hub


def reset_usb_hub(hub: UsbDevice) -> bool:
    """Reset the specified USB hub."""
    print(f"Resetting hub: {hub.name}")
    print(f"Bus/Device: {hub.bus_device}")

    try:
        result = subprocess.run(
            ["usbreset", hub.bus_device],
            capture_output=True,
            text=True,
            check=True,
        )
        print("Hub reset successful")
        if result.stdout:
            print(f"usbreset output: {result.stdout.strip()}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"ERROR: Failed to reset hub: {e}")
        if e.stderr:
            print(f"usbreset error: {e.stderr.strip()}")
        return False


# ==========================================
# MID-LEVEL PARSING
# ==========================================

def parse_lsusb_lookup(lsusb_output: str) -> dict[str, dict[str, str]]:
    """Parse standard lsusb into a lookup dictionary keyed by 'bus/device'."""
    lsusb_lookup = {}
    for line in lsusb_output.split("\n"):
        match = re.search(r"Bus (\d+) Device (\d+): ID ([0-9a-f]{4}):([0-9a-f]{4}) (.+)", line)
        if match:
            bus, dev, vid, pid, name = match.groups()
            lsusb_lookup[f"{bus}/{dev}"] = {
                "vendor_id": vid,
                "product_id": pid,
                "name": name.strip(),
                "full_line": line.strip()
            }
    return lsusb_lookup


def parse_usb_devices(tree_output: str, lsusb_output: str) -> list[UsbDevice]:
    """
    Parse both lsusb outputs into a structured list of UsbDevice objects,
    establishing parent/child hierarchy automatically.
    """
    lsusb_lookup = parse_lsusb_lookup(lsusb_output)

    # Pass 1: Extract pure structural sequence from the tree text
    flat_tree_sequence = parse_flat_tree_sequence(tree_output)

    # Pass 2: Assemble objects by merging the sequence with the lookup details
    return build_device_tree(flat_tree_sequence, lsusb_lookup)


def parse_flat_tree_sequence(tree_output: str) -> list[dict]:
    """Pass 1: Pure string parsing. Converts tree text into a structural sequence."""
    # The order of the flat tree sequence is important. Must be a list, not a dictionary.
    flat_data = []
    current_bus = ""

    for line in tree_output.split("\n"):
        if not line.strip():
            continue

        current_bus = extract_bus_number(line) or current_bus
        dev_num = extract_device_number(line)

        if dev_num:
            flat_data.append({
                "bus": current_bus,
                "dev_num": dev_num,
                "indent_level": get_indent_level(line),
                "is_hub": "Class=Hub" in line or "Class=root_hub" in line,
                "line": line.strip()
            })

    return flat_data


def build_device_tree(flat_tree_sequence: list[dict], lsusb_lookup: dict[str, dict[str, str]]) -> list[UsbDevice]:
    """Pass 2: Pure logic. Assembles UsbDevice objects and assigns parents."""
    devices: list[UsbDevice] = []
    parent_stack: list[tuple[int, UsbDevice]] = []
    created_devices: set[str] = set()

    for data in flat_tree_sequence:
        bus_device_key = f"{data['bus']}/{data['dev_num']}"

        # Interface deduplication: We only create the object if we haven't seen the physical device yet, otherwise devices with multiple interfaces in the lsusb output will be created multiple times.
        # Crucially, we skip stack modifications for duplicates, so the tree hierarchy stays intact!
        if bus_device_key in created_devices:
            continue

        created_devices.add(bus_device_key)
        details = lsusb_lookup.get(bus_device_key, {})

        device = UsbDevice(
            bus=data["bus"],
            device_num=data["dev_num"],
            vendor_id=details.get("vendor_id", ""),
            product_id=details.get("product_id", ""),
            name=details.get("name", "Unknown Device"),
            is_hub=data["is_hub"],
            indent_level=data["indent_level"],
            bus_device=bus_device_key,
            full_line=details.get("full_line", data["line"])
        )

        assign_parent(device, parent_stack)
        devices.append(device)
        parent_stack.append((device.indent_level, device))

    return devices


# ==========================================
# LOW-LEVEL PARSING UTILITIES
# ==========================================

def extract_bus_number(line: str) -> str | None:
    """Extract bus number from a tree line."""
    match = re.search(r"Bus (\d+)", line)
    return match.group(1) if match else None


def extract_device_number(line: str) -> str | None:
    """Extract device number from a tree line."""
    match = re.search(r"Dev (\d+)", line)
    return match.group(1) if match else None


def get_indent_level(line: str) -> int:
    """Get the indentation level of a line (number of leading spaces)."""
    return len(line) - len(line.lstrip())


def create_usb_device(line: str, bus: str, dev_num: str, lsusb_lookup: dict) -> UsbDevice:
    """Construct a UsbDevice object from parsed line data and lookups."""
    indent_level = get_indent_level(line)
    is_hub = "Class=Hub" in line or "Class=root_hub" in line
    bus_device_key = f"{bus}/{dev_num}"

    details = lsusb_lookup.get(bus_device_key, {})

    return UsbDevice(
        bus=bus,
        device_num=dev_num,
        vendor_id=details.get("vendor_id", ""),
        product_id=details.get("product_id", ""),
        name=details.get("name", "Unknown Device"),
        is_hub=is_hub,
        indent_level=indent_level,
        bus_device=bus_device_key,
        full_line=details.get("full_line", line.strip())
    )


def assign_parent(device: UsbDevice, parent_stack: list[tuple[int, UsbDevice]]) -> None:
    """Assigns the parent of a device based on current stack indentation depth."""
    while parent_stack and parent_stack[-1][0] >= device.indent_level:
        parent_stack.pop()

    if parent_stack:
        device.parent = parent_stack[-1][1]


# ==========================================
# ENTRY POINT
# ==========================================

if __name__ == "__main__":
    success = find_eyechip_and_reset_hub()
    sys.exit(0 if success else 1)

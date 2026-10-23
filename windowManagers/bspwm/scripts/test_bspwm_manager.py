# test_bspwm_daemon.py
"""
Comprehensive Pytest Suite for BSPWM Monitor & Desktop Daemon
Tests spatial mapping, proximity fallbacks, quota rules, naming,
event stream parsing, and end-to-end integration via mocked BSPWM/XRANDR interfaces.
"""

import pytest
from unittest.mock import patch, MagicMock, call
from bspwm_manager import WorkspaceManager, Monitor, Desktop, BspcClient, listen_for_events


@pytest.fixture
def manager():
    return WorkspaceManager()


# ==============================================================================
# 1. PARAMETERIZED SPATIAL LAYOUT & PREFIX TESTS
# ==============================================================================

XRANDR_SPATIAL_CASES = [
    (
        "two_right_monitors",
        """Monitors: 3
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-2 1920/509x1080/286+1920+0  DP-2
 2: +DP-3 1920/509x1080/286+3840+0  DP-3""",
        {"0x01": "DP-1", "0x02": "DP-2", "0x03": "DP-3"},
        {
            "0x01": ("primary", ""),
            "0x02": ("right_1", ">"),
            "0x03": ("right_2", ">>"),
        },
    ),
    (
        "inverted_left_monitor",
        """Monitors: 2
 0: +DP-1 1920/527x1080/296+0+0  DP-1
 1: +*DP-2 1920/509x1080/286+1920+0  DP-2""",
        {"0x01": "DP-1", "0x02": "DP-2"},
        {
            "0x02": ("primary", ""),
            "0x01": ("left_1", "<"),
        },
    ),
    (
        "vertical_above_negative_y_offset",
        """Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-4 1920/509x1080/286+0-1080  DP-4""",
        {"0x01": "DP-1", "0x04": "DP-4"},
        {
            "0x01": ("primary", ""),
            "0x04": ("above_1", "^"),
        },
    ),
    (
        "vertical_below_positive_y_offset",
        """Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-5 1920/509x1080/286+0+1080  DP-5""",
        {"0x01": "DP-1", "0x05": "DP-5"},
        {
            "0x01": ("primary", ""),
            "0x05": ("below_1", "v"),
        },
    ),
    (
        "surround_all_four_cardinal_directions",
        """Monitors: 5
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-2 1920/509x1080/286-1920+0  DP-2
 2: +DP-3 1920/509x1080/286+0-1080  DP-3
 3: +DP-4 1920/509x1080/286+1920+0  DP-4
 4: +DP-5 1920/509x1080/286+0+1080  DP-5""",
        {
            "0x01": "DP-1",
            "0x02": "DP-2",
            "0x03": "DP-3",
            "0x04": "DP-4",
            "0x05": "DP-5",
        },
        {
            "0x01": ("primary", ""),
            "0x02": ("left_1", "<"),
            "0x03": ("above_1", "^"),
            "0x04": ("right_1", ">"),
            "0x05": ("below_1", "v"),
        },
    ),
    (
        "laptop_fallback_without_asterisk",
        """Monitors: 1
 0: +eDP-1 1920/300x1200/200+0+0  eDP-1""",
        {"0x40": "eDP-1"},
        {
            "0x40": ("primary", ""),
        },
    ),
]


@pytest.mark.parametrize(
    "case_name,xrandr_out,monitor_map,expected_monitors", XRANDR_SPATIAL_CASES
)
@patch("bspwm_manager.BspcClient.get_monitor_map")
@patch("subprocess.run")
def test_spatial_layout_and_prefixes(
    mock_subproc,
    mock_map,
    manager,
    case_name,
    xrandr_out,
    monitor_map,
    expected_monitors,
):
    mock_subproc.return_value.stdout = xrandr_out
    mock_map.return_value = monitor_map

    manager.parse_xrandr()

    for m_id, (exp_slot, exp_prefix) in expected_monitors.items():
        assert m_id in manager.monitors, f"[{case_name}] Expected monitor {m_id} not found"
        mon = manager.monitors[m_id]
        assert mon.slot == exp_slot, f"[{case_name}] Slot mismatch for {m_id}"
        assert mon.prefix == exp_prefix, f"[{case_name}] Prefix mismatch for {m_id}"


# ==============================================================================
# 2. PARAMETERIZED PROXIMITY FALLBACK MATRIX TESTS
# ==============================================================================

FALLBACK_MATRIX_CASES = [
    (
        "exact_slot_match_available",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1", ">"),
            "0x03": Monitor("0x03", "DP-3", False, 3840, 0, 1920, 1080, "right_2", ">>"),
        },
        "0x01",
        "right_2",
        "0x03",
    ),
    (
        "same_axis_step_down_right2_to_right1",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1", ">"),
        },
        "0x01",
        "right_2",
        "0x02",
    ),
    (
        "cross_axis_fallback_to_any_secondary_over_primary",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 1080, 1920, 1080, "primary"),
            "0x04": Monitor("0x04", "DP-4", False, 0, 0, 1920, 1080, "above_1", "^"),
        },
        "0x01",
        "right_2",
        "0x04",
    ),
    (
        "collapse_to_primary_only_when_no_secondaries_remain",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
        },
        "0x01",
        "right_2",
        "0x01",
    ),
]


@pytest.mark.parametrize(
    "case_name,monitors_dict,pri_id,target_slot,expected_fallback_id",
    FALLBACK_MATRIX_CASES,
)
def test_proximity_fallback_matrix(
    manager, case_name, monitors_dict, pri_id, target_slot, expected_fallback_id
):
    manager.monitors = monitors_dict
    manager.primary_id = pri_id

    resolved_id = manager.get_fallback_monitor_id(target_slot)
    assert (
        resolved_id == expected_fallback_id
    ), f"[{case_name}] Expected fallback {expected_fallback_id}, got {resolved_id}"


# ==============================================================================
# 3. PARAMETERIZED DESKTOP NAMING & PREFIX TESTS
# ==============================================================================

NAMING_MATRIX_CASES = [
    (
        "single_primary_minimum_quota",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary", ""),
        },
        {"0x01": ["0xD1", "0xD2", "0xD3", "0xD4", "0xD5"]},
        {
            "0xD1": " ",
            "0xD2": " ",
            "0xD3": " ",
            "0xD4": "4",
            "0xD5": "5",
        },
    ),
    (
        "dual_monitor_primary_and_one_right",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary", ""),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1", ">"),
        },
        {
            "0x01": ["0xD1", "0xD2", "0xD3", "0xD4"],
            "0x02": ["0xD5", "0xD6", "0xD7", "0xD8"],
        },
        {
            "0xD1": " ",
            "0xD2": " ",
            "0xD3": " ",
            "0xD4": "4",
            "0xD5": ">5",
            "0xD6": " ",
            "0xD7": " ",
            "0xD8": ">8",
        },
    ),
    (
        "two_right_monitors_double_prefix",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary", ""),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1", ">"),
            "0x03": Monitor("0x03", "DP-3", False, 3840, 0, 1920, 1080, "right_2", ">>"),
        },
        {
            "0x01": ["0xD1", "0xD2", "0xD3", "0xD4"],
            "0x02": ["0xD5", "0xD6", "0xD7", "0xD8"],
            "0x03": ["0xD9", "0xDA"],
        },
        {
            "0xD1": " ",
            "0xD2": " ",
            "0xD3": " ",
            "0xD4": "4",
            "0xD5": ">5",
            "0xD6": " ",
            "0xD7": " ",
            "0xD8": ">8",
            "0xD9": ">>9",
            "0xDA": " ",
        },
    ),
    (
        "surround_all_cardinal_directions",
        {
            "0x02": Monitor("0x02", "DP-2", False, -1920, 0, 1920, 1080, "left_1", "<"),
            "0x03": Monitor("0x03", "DP-3", False, 0, -1080, 1920, 1080, "above_1", "^"),
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary", ""),
            "0x04": Monitor("0x04", "DP-4", False, 0, 1080, 1920, 1080, "below_1", "v"),
        },
        {
            "0x02": ["0xD1", "0xD2"],
            "0x03": ["0xD3", "0xD4"],
            "0x01": ["0xD5", "0xD6", "0xD7", "0xD8"],
            "0x04": ["0xD9", "0xDA"],
        },
        {
            "0xD1": "<1",
            "0xD2": " ",
            "0xD3": "^3",
            "0xD4": " ",
            "0xD5": " ",
            "0xD6": " ",
            "0xD7": " ",
            "0xD8": "8",
            "0xD9": "v9",
            "0xDA": " ",
        },
    ),
]


@pytest.mark.parametrize(
    "case_name,monitors_dict,monitor_desktops,expected_names", NAMING_MATRIX_CASES
)
@patch("bspwm_manager.BspcClient.run_cmd")
def test_desktop_naming_rules(
    mock_run_cmd, manager, case_name, monitors_dict, monitor_desktops, expected_names
):
    manager.monitors = monitors_dict
    manager.desktops = {
        d_id: Desktop(d_id, "", m_id)
        for m_id, d_ids in monitor_desktops.items()
        for d_id in d_ids
    }

    def bspc_stub(args):
        if args[:3] == ["query", "--desktops", "--monitor"]:
            m_id = args[3]
            return "\n".join(monitor_desktops.get(m_id, []))
        return ""

    mock_run_cmd.side_effect = bspc_stub

    manager.reorder_and_rename()

    for d_id, exp_name in expected_names.items():
        mock_run_cmd.assert_any_call(["desktop", d_id, "--rename", exp_name])
        assert manager.desktops[d_id].name == exp_name


# ==============================================================================
# 4. PARAMETERIZED QUOTA ENFORCEMENT TESTS
# ==============================================================================

QUOTA_CASES = [
    (
        "boot_dual_under_quota",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1"),
        },
        {"0x01": ["0xD1"], "0x02": ["0xD2"]},
        {"0x01": 3, "0x02": 1},
    ),
    (
        "reload_with_excess_desktops_preserved",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, "right_1"),
        },
        {
            "0x01": ["0xD1", "0xD2", "0xD3", "0xD4", "0xD5", "0xD6"],
            "0x02": ["0xD7", "0xD8", "0xD9"],
        },
        {},
    ),
    (
        "single_laptop_under_quota",
        {
            "0x40": Monitor("0x40", "eDP-1", True, 0, 0, 1920, 1080, "primary"),
        },
        {"0x40": ["0xD1", "0xD2"]},
        {"0x40": 2},
    ),
]


@pytest.mark.parametrize(
    "case_name,monitors_dict,current_desktops,expected_additions", QUOTA_CASES
)
@patch("bspwm_manager.BspcClient.add_desktops")
@patch("bspwm_manager.BspcClient.get_desktops")
def test_quota_enforcement(
    mock_get_desks,
    mock_add_desks,
    manager,
    case_name,
    monitors_dict,
    current_desktops,
    expected_additions,
):
    manager.monitors = monitors_dict
    mock_get_desks.side_effect = lambda m_id: current_desktops.get(m_id, [])

    with patch.object(manager, "sync_desktops"):
        manager.enforce_quotas()

    if not expected_additions:
        mock_add_desks.assert_not_called()
    else:
        for m_id, needed_count in expected_additions.items():
            mock_add_desks.assert_any_call(m_id, needed_count)


# ==============================================================================
# 5. PARAMETERIZED EVENT LINE DISPATCH TESTS
# ==============================================================================

EVENT_DISPATCH_CASES = [
    ("monitor_add 0x00800040 eDP-1 1920x1200+0+0\n", True),
    ("monitor_remove 0x00800004\n", True),
    ("monitor_geometry 0x00800002 1920x1080+0+0\n", True),
    ("monitor_swap 0x00800002 0x00800004\n", True),
    ("desktop_add 0x00800002 0x0080003D tmp_test\n", True),
    ("desktop_remove 0x00800002 0x0080003D\n", True),
    ("desktop_swap 0x00800004 0x0080003D 0x00800004 0x00800014\n", True),
    ("monitor_focus 0x00800002\n", False),
    ("desktop_focus 0x00800002 0x0080003D\n", False),
    ("node_focus 0x00800002 0x0080003D 0x00200001\n", False),
    ("   \n", False),
]


@pytest.mark.parametrize("event_line,expected_needs_cycle", EVENT_DISPATCH_CASES)
def test_subscription_event_line_dispatch(manager, event_line, expected_needs_cycle):
    needs_cycle = manager.process_event_line(event_line)
    assert (
        needs_cycle is expected_needs_cycle
    ), f"Event line failed expectation: {event_line.strip()}"


# ==============================================================================
# 6. REPOSITIONING & SAFE DECOMMISSIONING UNIT TESTS
# ==============================================================================

def test_monitor_geometry_repositioning_recalculates_prefix(manager):
    left_state = """Monitors: 2
 0: +*DP-1 1920/527x1080/296+1920+0  DP-1
 1: +DP-2 1920/509x1080/286+0+0  DP-2"""

    right_state = """Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-2 1920/509x1080/286+1920+0  DP-2"""

    mock_map = {"0x01": "DP-1", "0x02": "DP-2"}

    with patch("bspwm_manager.BspcClient.get_monitor_map", return_value=mock_map):
        with patch("subprocess.run") as mock_subproc:
            mock_subproc.return_value.stdout = left_state
            manager.parse_xrandr()
            assert manager.monitors["0x02"].slot == "left_1"
            assert manager.monitors["0x02"].prefix == "<"

            mock_subproc.return_value.stdout = right_state
            manager.parse_xrandr()
            assert manager.monitors["0x02"].slot == "right_1"
            assert manager.monitors["0x02"].prefix == ">"


@patch("bspwm_manager.BspcClient.run_cmd")
def test_safe_evacuation_leaves_one_desktop_and_purges_ghosts(mock_run_cmd, manager):
    manager.monitors = {
        "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, "primary"),
    }
    manager.primary_id = "0x01"

    manager.desktops = {
        "0xD1": Desktop("0xD1", " ", "0x01", "primary"),
        "0xD8": Desktop("0xD8", ">5", "0x02", "right_1"),
        "0xD9": Desktop("0xD9", " ", "0x02", "right_1"),
    }

    def bspc_stub(args):
        if args == ["query", "--monitors"]:
            return "0x01\n0x02"
        if args[:3] == ["query", "--desktops", "--monitor"]:
            return "0xD1" if args[3] == "0x01" else "0xD8\n0xD9"
        if args[:3] == ["query", "--monitors", "--desktop"]:
            return "0x01"
        return ""

    mock_run_cmd.side_effect = bspc_stub

    manager.route_desktops()

    mock_run_cmd.assert_any_call(["desktop", "0xD8", "--to-monitor", "0x01"])
    assert call(["desktop", "0xD9", "--to-monitor", "0x01"]) not in mock_run_cmd.call_args_list
    mock_run_cmd.assert_any_call(["monitor", "0x02", "--remove"])

    def bspc_sync_stub(args):
        if args == ["query", "--monitors"]:
            return "0x01"
        if args[:3] == ["query", "--desktops", "--monitor"]:
            return "0xD1"
        return ""

    mock_run_cmd.side_effect = bspc_sync_stub
    manager.sync_desktops()

    assert "0xD8" not in manager.desktops
    assert "0xD9" not in manager.desktops
    assert "0xD1" in manager.desktops


# ==============================================================================
# 7. INTEGRATION TESTS (REAL SUBSCRIPTION STREAM & LIFECYCLE)
# ==============================================================================

REAL_EVENT_STREAM = [
    "desktop_add 0x00800002 0x0080003D tmp_event_test\n",
    "desktop_transfer 0x00800002 0x0080003D 0x00800004\n",
    "desktop_swap 0x00800004 0x0080003D 0x00800004 0x00800014\n",
    "desktop_transfer 0x00800004 0x0080003D 0x00800002\n",
    "desktop_remove 0x00800002 0x0080003D\n",
    "monitor_focus 0x00800004\n",
    "monitor_geometry 0x00800002 1920x1080+0+0\n",
    "monitor_add 0x00800040 eDP-1 1920x1200+0+0\n",
    "monitor_remove 0x00800004\n",
]


@patch("bspwm_manager.time.sleep")
@patch("bspwm_manager.WorkspaceManager.run_cycle")
@patch("bspwm_manager.WorkspaceManager.handle_transfer")
@patch("subprocess.Popen")
def test_real_subscription_event_stream_parsing(
    mock_popen, mock_handle_transfer, mock_run_cycle, mock_sleep, manager
):
    mock_proc = MagicMock()
    mock_proc.stdout = REAL_EVENT_STREAM
    mock_popen.return_value = mock_proc

    listen_for_events(manager)

    assert mock_run_cycle.call_count == 6
    assert mock_handle_transfer.call_args_list == [
        call("0x0080003D", "0x00800004"),
        call("0x0080003D", "0x00800002"),
    ]


@patch("bspwm_manager.BspcClient.run_cmd")
def test_integration_event_desktop_transfer_to_bspc_rename(mock_run_cmd, manager):
    manager.monitors = {
        "0x00800002": Monitor("0x00800002", "DP-3-8", True, 0, 0, 1920, 1080, "primary", ""),
        "0x00800004": Monitor("0x00800004", "DP-3-1", False, 1920, 0, 1920, 1080, "right_1", ">"),
    }
    manager.desktops = {
        "0xD1": Desktop("0xD1", " ", "0x00800002", "primary"),
        "0xD2": Desktop("0xD2", " ", "0x00800002", "primary"),
        "0xD3": Desktop("0xD3", " ", "0x00800002", "primary"),
        "0xD4": Desktop("0xD4", "4", "0x00800002", "primary"),
        "0x0080003D": Desktop("0x0080003D", "tmp_test", "0x00800002", "primary"),
        "0xD6": Desktop("0xD6", " ", "0x00800004", "right_1"),
    }

    def bspc_stub(args):
        if args[:3] == ["query", "--desktops", "--monitor"]:
            m_id = args[3]
            return (
                "0xD1\n0xD2\n0xD3\n0xD4"
                if m_id == "0x00800002"
                else "0x0080003D\n0xD6"
            )
        return ""

    mock_run_cmd.side_effect = bspc_stub

    manager.process_event_line("desktop_transfer 0x00800002 0x0080003D 0x00800004\n")

    assert manager.desktops["0x0080003D"].preferred_slot == "right_1"
    assert manager.desktops["0x0080003D"].current_monitor_id == "0x00800004"

    mock_run_cmd.assert_any_call(["wm", "--reorder-monitors", "0x00800002", "0x00800004"])
    mock_run_cmd.assert_any_call(["desktop", "0x0080003D", "--rename", ">5"])
    mock_run_cmd.assert_any_call(["desktop", "0xD6", "--rename", " "])


@patch("subprocess.run")
@patch("bspwm_manager.BspcClient.get_monitor_map")
@patch("bspwm_manager.BspcClient.run_cmd")
def test_full_daemon_lifecycle_integration(
    mock_bspc_cmd, mock_get_map, mock_xrandr, manager
):
    """
    Full End-to-End Daemon Lifecycle Simulation:
    Phase 1: Boot Single Monitor (DP-3-8) -> Enforce Quota (create missing)
    Phase 2: Hotplug Secondary Monitor (DP-3-1) -> Detect right_1, enforce quota, rename '>5'
    Phase 3: Transfer desktop -> Intercept Hex ID and record memory
    Phase 4: USB-C Unplug Burst -> Evacuate to eDP-1, preserve preferred slot, rename on single screen
    """

    # Track simulated monitor-to-desktop state dynamically
    state = {
        "monitors": ["0x00800002"],
        "desktops": {
            "0x00800002": ["0xD1", "0xD2"]
        }
    }

    def dynamic_bspc(args):
        if args == ["query", "--monitors"]:
            return "\n".join(state["monitors"])
        if args[:3] == ["query", "--desktops", "--monitor"]:
            m_id = args[3]
            return "\n".join(state["desktops"].get(m_id, []))
        if len(args) == 4 and args[0] == "desktop" and args[2] == "--to-monitor":
            d_id, target_m = args[1], args[3]
            for m, desks in state["desktops"].items():
                if d_id in desks:
                    desks.remove(d_id)
            state["desktops"].setdefault(target_m, []).append(d_id)
            return ""
        if len(args) == 3 and args[0] == "monitor" and args[2] == "--remove":
            rem_m = args[1]
            if rem_m in state["monitors"]:
                state["monitors"].remove(rem_m)
            # When bspwm removes a monitor, remaining desktops migrate to the active monitor
            remaining = state["desktops"].pop(rem_m, [])
            if state["monitors"]:
                target_m = state["monitors"][0]
                state["desktops"].setdefault(target_m, []).extend(remaining)
            return ""
        if args[:3] == ["query", "--monitors", "--desktop"]:
            d_id = args[3]
            for m, desks in state["desktops"].items():
                if d_id in desks:
                    return m
            return ""
        return ""

    mock_bspc_cmd.side_effect = dynamic_bspc

    # --- PHASE 1: Boot Single Monitor (DP-3-8) ---
    mock_get_map.return_value = {"0x00800002": "DP-3-8"}
    mock_xrandr.return_value.stdout = """Monitors: 1
 0: +*DP-3-8 1920/527x1080/296+0+0  DP-3-8"""

    manager.run_cycle()

    add_calls = [
        c for c in mock_bspc_cmd.call_args_list
        if len(c[0]) > 0 and c[0][0][:3] == ["monitor", "0x00800002", "--add-desktops"]
    ]
    assert len(add_calls) == 1
    assert len(add_calls[0][0][0][3:]) == 2  # Added 2 to reach quota of 4

    # Simulate added desktops taking effect in BSPWM
    state["desktops"]["0x00800002"].extend(["0xD3", "0xD4"])

    # --- PHASE 2: Hotplug Secondary Monitor (DP-3-1) ---
    mock_bspc_cmd.reset_mock()
    mock_get_map.return_value = {
        "0x00800002": "DP-3-8",
        "0x00800004": "DP-3-1",
    }
    mock_xrandr.return_value.stdout = """Monitors: 2
 0: +*DP-3-8 1920/527x1080/296+0+0  DP-3-8
 1: +DP-3-1 1920/509x1080/286+1920+0  DP-3-1"""

    state["monitors"].append("0x00800004")
    state["desktops"]["0x00800004"] = ["0xD5"]  # Starts with 1 desktop; quota is 2

    manager.run_cycle()

    sec_add_calls = [
        c for c in mock_bspc_cmd.call_args_list
        if len(c[0]) > 0 and c[0][0][:3] == ["monitor", "0x00800004", "--add-desktops"]
    ]
    assert len(sec_add_calls) == 1
    assert len(sec_add_calls[0][0][0][3:]) == 1  # Added 1 to reach quota of 2
    mock_bspc_cmd.assert_any_call(["desktop", "0xD5", "--rename", ">5"])

    # --- PHASE 3: Manual Transfer Interception ---
    state["desktops"]["0x00800002"].remove("0xD4")
    state["desktops"]["0x00800004"].append("0xD4")

    manager.handle_transfer("0xD4", "0x00800004")
    assert manager.desktops["0xD4"].preferred_slot == "right_1"
    assert manager.desktops["0xD4"].current_monitor_id == "0x00800004"

    # --- PHASE 4: USB-C Unplug Burst (Dual External Off, Laptop eDP-1 On) ---
    mock_bspc_cmd.reset_mock()
    mock_get_map.return_value = {"0x00800040": "eDP-1"}
    mock_xrandr.return_value.stdout = """Monitors: 1
 0: +*eDP-1 1920/300x1200/200+0+0  eDP-1"""

    # BSPWM reports eDP-1 plugged in, while disconnected external screens linger until daemon cleanup
    state["monitors"].append("0x00800040")
    state["desktops"]["0x00800040"] = ["0xD42"]

    burst_events = [
        "monitor_geometry 0x00800002 1920x1080+0+0\n",
        "monitor_geometry 0x00800004 1920x1080+1920+0\n",
        "monitor_add 0x00800040 eDP-1 1920x1200+0+0\n",
    ]
    for evt in burst_events:
        manager.process_event_line(evt)

    manager.run_cycle()

    # Evacuations executed via move_desktop:
    # 0x00800002 has [0xD1, 0xD2, 0xD3] -> 0xD1, 0xD2 evacuated (0xD3 left for monitor removal)
    # 0x00800004 has [0xD5, 0xD4] -> 0xD5 evacuated (0xD4 left for monitor removal)
    mock_bspc_cmd.assert_any_call(["desktop", "0xD1", "--to-monitor", "0x00800040"])
    mock_bspc_cmd.assert_any_call(["desktop", "0xD2", "--to-monitor", "0x00800040"])
    mock_bspc_cmd.assert_any_call(["desktop", "0xD5", "--to-monitor", "0x00800040"])

    # Disconnected monitors decommissioned
    mock_bspc_cmd.assert_any_call(["monitor", "0x00800002", "--remove"])
    mock_bspc_cmd.assert_any_call(["monitor", "0x00800004", "--remove"])

    # First three desktops on primary are blank; subsequent ones receive sequential numbers
    mock_bspc_cmd.assert_any_call(["desktop", "0xD42", "--rename", " "])
    mock_bspc_cmd.assert_any_call(["desktop", "0xD1", "--rename", " "])
    mock_bspc_cmd.assert_any_call(["desktop", "0xD5", "--rename", "4"])

    # Spatial preference preserved across physical disconnection
    assert manager.desktops["0xD4"].preferred_slot == "right_1"

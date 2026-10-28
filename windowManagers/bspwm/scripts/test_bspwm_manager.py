#!/usr/bin/env python3
"""
Comprehensive Pytest Suite for Unified BSPWM Manager
Tests spatial mapping, proximity fallbacks, quota rules, naming,
and end-to-end integration driven by mocked JSON state dumps.
"""

import json
import pytest
from unittest.mock import MagicMock, call, patch
from bspwm_manager import WorkspaceManager, Monitor, Desktop, BspcClient


@pytest.fixture
def manager():
    return WorkspaceManager()


def make_wm_dump(primary_id_int: int, monitors_info: list) -> dict:
    """Helper to generate a mock bspwm JSON state dump.
    monitors_info: [(m_name, m_id_int, [(d_name, d_id_int, has_nodes)])]
    """
    return {
        "primaryMonitorId": primary_id_int,
        "monitors": [
            {
                "name": m_name,
                "id": m_id,
                "desktops": [
                    {
                        "name": d_name,
                        "id": d_id,
                        "root": {"id": 999} if has_nodes else None,
                    }
                    for d_name, d_id, has_nodes in d_info
                ],
            }
            for m_name, m_id, d_info in monitors_info
        ],
    }


# ==============================================================================
# 0. RAW JSON PARSING VALIDATION (From Original Dump)
# ==============================================================================

SAMPLE_DUAL_MONITOR_DUMP = json.loads("""{
  "focusedMonitorId": 8388965, "primaryMonitorId": 8388965,
  "monitors": [
    {
      "name": "DP-3-8", "id": 8388965, "rectangle": {"x": 0, "y": 0, "width": 1920, "height": 1080},
      "desktops": [
        {"name": " ", "id": 8388614, "root": {"id": 4194318}},
        {"name": "8", "id": 8388992, "root": null}
      ]
    },
    {
      "name": "DP-3-1", "id": 8388967, "rectangle": {"x": 1920, "y": 0, "width": 1920, "height": 1080},
      "desktops": [
        {"name": ">9", "id": 8388970, "root": {"id": 8388993}},
        {"name": " ", "id": 8388971, "root": null}
      ]
    }
  ]
}""")

@patch("bspwm_manager.BspcClient.get_wm_dump")
@patch("subprocess.run")
def test_raw_json_dump_parser(mock_run, mock_get_dump, manager):
    mock_get_dump.return_value = SAMPLE_DUAL_MONITOR_DUMP
    mock_run.return_value.stdout = "Monitors: 2\n 0: +*DP-3-8 1920x1080+0+0 DP-3-8\n 1: +DP-3-1 1920x1080+1920+0 DP-3-1"

    manager.ingest_live_state()

    assert len(manager.monitors) == 2

    dp38 = manager.monitors["0x00800165"]
    assert dp38.name == "DP-3-8"
    assert dp38.is_primary is True
    assert len(dp38.desktop_ids) == 2
    assert manager.desktops["0x00800006"].has_nodes is True
    assert manager.desktops["0x00800180"].has_nodes is False  # Desktop 8 was empty in the dump

    dp31 = manager.monitors["0x00800167"]
    assert dp31.name == "DP-3-1"
    assert dp31.is_primary is False
    assert len(dp31.desktop_ids) == 2


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
        [("DP-1", 1, []), ("DP-2", 2, []), ("DP-3", 3, [])], 1,
        {"0x00000001": ("primary", ""), "0x00000002": ("right_1", ">"), "0x00000003": ("right_2", ">>")},
    ),
    (
        "inverted_left_monitor",
        """Monitors: 2
 0: +DP-1 1920/527x1080/296+0+0  DP-1
 1: +*DP-2 1920/509x1080/286+1920+0  DP-2""",
        [("DP-1", 1, []), ("DP-2", 2, [])], 2,
        {"0x00000002": ("primary", ""), "0x00000001": ("left_1", "<")},
    ),
    (
        "vertical_above_negative_y_offset",
        """Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-4 1920/509x1080/286+0-1080  DP-4""",
        [("DP-1", 1, []), ("DP-4", 4, [])], 1,
        {"0x00000001": ("primary", ""), "0x00000004": ("above_1", "^")},
    ),
    (
        "vertical_below_positive_y_offset",
        """Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-5 1920/509x1080/286+0+1080  DP-5""",
        [("DP-1", 1, []), ("DP-5", 5, [])], 1,
        {"0x00000001": ("primary", ""), "0x00000005": ("below_1", "v")},
    ),
    (
        "surround_all_four_cardinal_directions",
        """Monitors: 5
 0: +*DP-1 1920/527x1080/296+0+0  DP-1
 1: +DP-2 1920/509x1080/286-1920+0  DP-2
 2: +DP-3 1920/509x1080/286+0-1080  DP-3
 3: +DP-4 1920/509x1080/286+1920+0  DP-4
 4: +DP-5 1920/509x1080/286+0+1080  DP-5""",
        [("DP-1", 1, []), ("DP-2", 2, []), ("DP-3", 3, []), ("DP-4", 4, []), ("DP-5", 5, [])], 1,
        {
            "0x00000001": ("primary", ""), "0x00000002": ("left_1", "<"),
            "0x00000003": ("above_1", "^"), "0x00000004": ("right_1", ">"), "0x00000005": ("below_1", "v"),
        },
    ),
    (
        "laptop_fallback_without_asterisk",
        """Monitors: 1\n 0: +eDP-1 1920/300x1200/200+0+0  eDP-1""",
        [("eDP-1", 64, [])], 0,  # bspwm sometimes returns 0 if primary isn't explicit
        {"0x00000040": ("primary", "")},
    ),
]

@pytest.mark.parametrize("case_name,xrandr_out,bsp_monitors,pri_id,expected_slots", XRANDR_SPATIAL_CASES)
@patch("bspwm_manager.BspcClient.get_wm_dump")
@patch("subprocess.run")
def test_spatial_layout_and_prefixes(mock_run, mock_get_dump, manager, case_name, xrandr_out, bsp_monitors, pri_id, expected_slots):
    mock_run.return_value.stdout = xrandr_out
    mock_get_dump.return_value = make_wm_dump(pri_id, bsp_monitors)

    manager.ingest_live_state()

    for m_hex, (exp_slot, exp_prefix) in expected_slots.items():
        assert m_hex in manager.monitors, f"[{case_name}] Expected monitor {m_hex} not found"
        mon = manager.monitors[m_hex]
        assert mon.slot == exp_slot, f"[{case_name}] Slot mismatch for {m_hex}"
        assert mon.prefix == exp_prefix, f"[{case_name}] Prefix mismatch for {m_hex}"


# ==============================================================================
# 2. PROXIMITY FALLBACK MATRIX TESTS
# ==============================================================================

FALLBACK_MATRIX_CASES = [
    ("exact_slot_match_available", "right_2", "0x03"),
    ("same_axis_step_down_right3_to_right2", "right_3", "0x03"),
    ("cross_axis_fallback_to_any_secondary_over_primary", "above_1", "0x02"),
]

@pytest.mark.parametrize("case_name,target_slot,expected_fallback", FALLBACK_MATRIX_CASES)
def test_proximity_fallback_matrix(manager, case_name, target_slot, expected_fallback):
    manager.monitors = {
        "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, [], "primary", ""),
        "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, [], "right_1", ">"),
        "0x03": Monitor("0x03", "DP-3", False, 3840, 0, 1920, 1080, [], "right_2", ">>"),
    }
    manager.primary_id = "0x01"
    assert manager.get_fallback_monitor_id(target_slot) == expected_fallback


def test_fallback_collapses_to_primary_when_no_secondaries_exist(manager):
    manager.monitors = {"0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, [], "primary", "")}
    manager.primary_id = "0x01"
    assert manager.get_fallback_monitor_id("right_2") == "0x01"


# ==============================================================================
# 3. RENAMING & QUOTA MATRIX TESTS
# ==============================================================================

NAMING_MATRIX_CASES = [
    (
        "single_primary_minimum_quota",
        {"0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, ["0xD1", "0xD2", "0xD3", "0xD4", "0xD5"], "primary", "")},
        {"0xD1": " ", "0xD2": " ", "0xD3": " ", "0xD4": "4", "0xD5": "5"},
    ),
    (
        "dual_monitor_primary_and_one_right",
        {
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, ["0xD1", "0xD2", "0xD3", "0xD4"], "primary", ""),
            "0x02": Monitor("0x02", "DP-2", False, 1920, 0, 1920, 1080, ["0xD5", "0xD6", "0xD7", "0xD8"], "right_1", ">"),
        },
        {"0xD1": " ", "0xD4": "4", "0xD5": ">5", "0xD6": " ", "0xD7": " ", "0xD8": ">8"},
    ),
    (
        "surround_all_cardinal_directions",
        {
            "0x02": Monitor("0x02", "DP-2", False, -1920, 0, 1920, 1080, ["0xD1", "0xD2"], "left_1", "<"),
            "0x03": Monitor("0x03", "DP-3", False, 0, -1080, 1920, 1080, ["0xD3", "0xD4"], "above_1", "^"),
            "0x01": Monitor("0x01", "DP-1", True, 0, 0, 1920, 1080, ["0xD5", "0xD6", "0xD7", "0xD8"], "primary", ""),
            "0x04": Monitor("0x04", "DP-4", False, 0, 1080, 1920, 1080, ["0xD9", "0xDA"], "below_1", "v"),
        },
        {"0xD1": "<1", "0xD2": " ", "0xD3": "^3", "0xD4": " ", "0xD5": " ", "0xD8": "8", "0xD9": "v9", "0xDA": " "},
    ),
]

@pytest.mark.parametrize("case_name,monitors_dict,expected_names", NAMING_MATRIX_CASES)
@patch("bspwm_manager.BspcClient.run_cmd")
def test_desktop_naming_rules(mock_run_cmd, manager, case_name, monitors_dict, expected_names):
    manager.monitors = monitors_dict
    manager.desktops = {
        d_id: Desktop(d_id, "", m_id)
        for m_id, mon in monitors_dict.items() for d_id in mon.desktop_ids
    }

    manager.reorder_and_rename()

    for d_id, exp_name in expected_names.items():
        mock_run_cmd.assert_any_call(["desktop", d_id, "--rename", exp_name])
        assert manager.desktops[d_id].name == exp_name


QUOTA_CASES = [
    ("boot_dual_under_quota", {"0x01": (True, 1), "0x02": (False, 1)}, {"0x01": 3, "0x02": 1}),
    ("reload_with_excess_desktops", {"0x01": (True, 6), "0x02": (False, 3)}, {}),
]

@pytest.mark.parametrize("case_name,monitor_states,expected_additions", QUOTA_CASES)
@patch("bspwm_manager.BspcClient.add_desktops")
@patch("bspwm_manager.WorkspaceManager.ingest_live_state")
def test_quota_enforcement(mock_ingest, mock_add_desks, manager, case_name, monitor_states, expected_additions):
    manager.monitors = {
        m_id: Monitor(m_id, f"DP-{m_id}", is_pri, 0, 0, 1920, 1080, [f"D{i}" for i in range(desk_cnt)], "primary" if is_pri else "right_1")
        for m_id, (is_pri, desk_cnt) in monitor_states.items()
    }

    manager.enforce_quotas()

    if not expected_additions:
        mock_add_desks.assert_not_called()
    else:
        for m_id, needed_count in expected_additions.items():
            mock_add_desks.assert_any_call(m_id, needed_count)


# ==============================================================================
# 4. EVENT LINE DISPATCH TESTS
# ==============================================================================

EVENT_DISPATCH_CASES = [
    ("monitor_add 0x00800040 eDP-1 1920x1200+0+0\n", True),
    ("monitor_remove 0x00800004\n", True),
    ("monitor_geometry 0x00800002 1920x1080+0+0\n", True),
    ("desktop_add 0x00800002 0x0080003D tmp_test\n", True),
    ("desktop_remove 0x00800002 0x0080003D\n", True),
    ("monitor_focus 0x00800002\n", False),
    ("desktop_focus 0x00800002 0x0080003D\n", False),
    ("node_focus 0x00800002 0x0080003D 0x00200001\n", False),
    ("   \n", False),
]

@pytest.mark.parametrize("event_line,expected_needs_cycle", EVENT_DISPATCH_CASES)
def test_subscription_event_line_dispatch(manager, event_line, expected_needs_cycle):
    assert manager.process_event_line(event_line) is expected_needs_cycle


# ==============================================================================
# 5. FULL DAEMON LIFECYCLE INTEGRATION (DRIVEN BY JSON MOCK STATE)
# ==============================================================================

@patch("subprocess.run")
@patch("bspwm_manager.BspcClient.get_wm_dump")
@patch("bspwm_manager.BspcClient.run_cmd")
def test_full_daemon_lifecycle_integration(
    mock_run_cmd, mock_get_dump, mock_xrandr, manager
):
    """
    Phase 1: Boot Single Monitor -> Enforces Quota
    Phase 2: Hotplug Secondary -> Enforces Quota, Renames '>5'
    Phase 3: Transfer desktop -> Updates preferred slot memory
    Phase 4: USB-C Unplug Burst -> Evacuates orphaned desktops safely
    """

    # --- PHASE 1: Boot Single Monitor ---
    mock_xrandr.return_value.stdout = "Monitors: 1\n 0: +*DP-1 1920x1080+0+0 DP-1"
    mock_get_dump.return_value = make_wm_dump(
        1, [("DP-1", 1, [(" ", 11, False), (" ", 12, False)])] # 2 desks, needs 4
    )

    manager.run_cycle()

    # Assert quotas enforced (check prefix and arg length to bypass dynamic timestamps)
    add_dp1 = [c[0][0] for c in mock_run_cmd.call_args_list if c[0][0][:3] == ["monitor", "0x00000001", "--add-desktops"]]
    assert len(add_dp1) == 1
    assert len(add_dp1[0]) == 5  # "monitor" "id" "--add-desktops" + 2 names


    # --- PHASE 2: Hotplug Secondary Monitor ---
    mock_run_cmd.reset_mock()
    mock_xrandr.return_value.stdout = "Monitors: 2\n 0: +*DP-1 1920x1080+0+0 DP-1\n 1: +DP-2 1920x1080+1920+0 DP-2"
    mock_get_dump.return_value = make_wm_dump(
        1, [
            ("DP-1", 1, [(" ", 11, False), (" ", 12, False), (" ", 13, False), ("4", 14, False)]),
            ("DP-2", 2, [("tmp", 21, False)]) # 1 desk, needs 2
        ]
    )

    manager.run_cycle()

    # Assert secondary quota enforced and desktop correctly prefixed as >5
    add_dp2 = [c[0][0] for c in mock_run_cmd.call_args_list if c[0][0][:3] == ["monitor", "0x00000002", "--add-desktops"]]
    assert len(add_dp2) == 1
    assert len(add_dp2[0]) == 4  # "monitor" "id" "--add-desktops" + 1 name
    mock_run_cmd.assert_any_call(["desktop", "0x00000015", "--rename", ">5"])


    # --- PHASE 3: Manual Transfer Interception ---
    mock_run_cmd.reset_mock()
    mock_get_dump.return_value = make_wm_dump(
        1, [
            ("DP-1", 1, [(" ", 11, False), (" ", 12, False), (" ", 13, False)]),
            ("DP-2", 2, [(">5", 21, False), (" ", 22, False), ("4", 14, False)])
        ]
    )
    manager.handle_transfer("0x0000000e", "0x00000002")  # 14 -> Hex 0E

    # Assert memory slot updated correctly
    assert manager.desktops["0x0000000e"].preferred_slot == "right_1"
    assert manager.desktops["0x0000000e"].current_monitor_id == "0x00000002"


    # --- PHASE 4: USB-C Unplug Burst (Laptops eDP-1 Takes Over) ---
    mock_run_cmd.reset_mock()
    mock_xrandr.return_value.stdout = "Monitors: 1\n 0: +*eDP-1 1920x1200+0+0 eDP-1"

    dump_phase4_initial = make_wm_dump(
        64, [
            ("DP-1", 1, [(" ", 11, False), (" ", 12, False), (" ", 13, False)]),
            ("DP-2", 2, [(">5", 21, False), (" ", 22, False), ("4", 14, False)]),
            ("eDP-1", 64, [("Desktop", 641, False)])
        ]
    )
    dump_phase4_routed = make_wm_dump(
        64, [
            ("eDP-1", 64, [
                ("Desktop", 641, False),
                (" ", 11, False), (" ", 12, False), (" ", 13, False),
                (">5", 21, False), (" ", 22, False), ("4", 14, False)
            ])
        ]
    )

    # run_cycle queries dump -> phase4_initial
    # route_desktops queries dump after evac -> phase4_routed
    # enforce_quotas / rename query dump -> phase4_routed
    mock_get_dump.side_effect = [
        dump_phase4_initial,
        dump_phase4_routed,
        dump_phase4_routed,
        dump_phase4_routed
    ]

    manager.run_cycle()

    # Assert Dummies added for safe evacuation
    mock_run_cmd.assert_any_call(["monitor", "0x00000001", "--add-desktops", "__evac_dummy__"])
    mock_run_cmd.assert_any_call(["monitor", "0x00000002", "--add-desktops", "__evac_dummy__"])

    # Assert all real desktops evacuated to eDP-1 fallback
    mock_run_cmd.assert_any_call(["desktop", "0x0000000b", "--to-monitor", "0x00000040"]) # 11
    mock_run_cmd.assert_any_call(["desktop", "0x0000000e", "--to-monitor", "0x00000040"]) # 14

    # Assert dead monitors removed
    mock_run_cmd.assert_any_call(["monitor", "0x00000001", "--remove"])
    mock_run_cmd.assert_any_call(["monitor", "0x00000002", "--remove"])

    # Assert placeholder 'Desktop' on eDP-1 was successfully pruned
    mock_run_cmd.assert_any_call(["desktop", "0x00000281", "--remove"]) # 641

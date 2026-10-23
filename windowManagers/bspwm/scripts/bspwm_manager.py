#!/usr/bin/env python3
"""
BSPWM Dynamic Monitor & Desktop Manager (ID-First Architecture)
Tracks and manipulates monitors and workspaces strictly via bspwm Hex IDs.
"""

import subprocess
import re
import sys
import time
import logging
import argparse
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple


@dataclass
class Monitor:
    hex_id: str
    name: str
    is_primary: bool
    x: int
    y: int
    width: int
    height: int
    slot: str = ""
    prefix: str = ""


@dataclass
class Desktop:
    hex_id: str
    name: str
    current_monitor_id: str
    preferred_slot: str = "primary"


class BspcClient:
    @staticmethod
    def run_cmd(args: List[str]) -> str:
        cmd_str = ' '.join(["bspc"] + args)
        logging.debug(f"Exec: {cmd_str}")
        try:
            res = subprocess.run(
                ["bspc"] + args, capture_output=True, text=True, check=True
            )
            return res.stdout.rstrip('\n')
        except subprocess.CalledProcessError as e:
            err_msg = e.stderr.strip()
            # Silently ignore redundant operations (exit code 1 with empty stderr)
            if e.returncode == 1 and not err_msg:
                logging.debug(f"Redundant command ignored seamlessly: {cmd_str}")
                return ""
            logging.warning(f"bspc command failed: {cmd_str} - {err_msg}")
            return ""

    @staticmethod
    def get_monitor_ids() -> List[str]:
        return BspcClient.run_cmd(["query", "--monitors"]).splitlines()

    @staticmethod
    def get_monitor_map() -> Dict[str, str]:
        """Pairs bspc query -M and bspc query -M --names to construct an ID <-> Name map."""
        ids = BspcClient.run_cmd(["query", "--monitors"]).splitlines()
        names = BspcClient.run_cmd(["query", "--monitors", "--names"]).splitlines()
        return dict(zip(ids, names))

    @staticmethod
    def get_desktops(monitor_id: str) -> List[str]:
        return BspcClient.run_cmd(["query", "--desktops", "--monitor", monitor_id]).splitlines()

    @staticmethod
    def get_desktop_name(desktop_id: str) -> str:
        return BspcClient.run_cmd(["query", "--desktops", "--desktop", desktop_id, "--names"])

    @staticmethod
    def move_desktop(desktop_id: str, monitor_id: str):
        BspcClient.run_cmd(["desktop", desktop_id, "--to-monitor", monitor_id])

    @staticmethod
    def rename_desktop(desktop_id: str, new_name: str):
        BspcClient.run_cmd(["desktop", desktop_id, "--rename", new_name])

    @staticmethod
    def add_desktops(monitor_id: str, count: int):
        if count > 0:
            names = [f"tmp_{monitor_id}_{i}_{int(time.time())}" for i in range(count)]
            BspcClient.run_cmd(["monitor", monitor_id, "--add-desktops"] + names)

    @staticmethod
    def remove_monitor(monitor_id: str):
        BspcClient.run_cmd(["monitor", monitor_id, "--remove"])

    @staticmethod
    def reorder_monitors(monitor_ids: List[str]):
        if monitor_ids:
            BspcClient.run_cmd(["wm", "--reorder-monitors"] + monitor_ids)


class WorkspaceManager:
    def __init__(self):
        # Keyed strictly by monitor Hex ID (e.g. '0x00800002')
        self.monitors: Dict[str, Monitor] = {}
        # Keyed strictly by desktop Hex ID (e.g. '0x0080003D')
        self.desktops: Dict[str, Desktop] = {}
        self.primary_id: str = ""

    def parse_xrandr(self):
        """Constructs spatial layout by linking xrandr output to bspwm monitor Hex IDs."""
        logging.debug("Parsing xrandr layout...")
        output = subprocess.run(["xrandr", "--listactivemonitors"], capture_output=True, text=True).stdout

        id_to_name = BspcClient.get_monitor_map()
        name_to_id = {name: m_id for m_id, name in id_to_name.items()}

        raw_monitors = []
        for line in output.splitlines():
            if not line.strip() or line.startswith("Monitors:"):
                continue

            parts = line.split()
            if len(parts) < 4:
                continue

            name = parts[-1]
            is_primary = "*" in parts[1]

            raw_geom = re.sub(r'/[0-9]+', '', parts[2])
            # Matches standard +X+Y, -X+Y, +X-Y, as well as +-X+-Y compound signs
            geom_match = re.search(r'(\d+)x(\d+)[+]?(-?\d+)[+]?(-?\d+)', raw_geom)

            if geom_match and name in name_to_id:
                w, h, x, y = map(int, geom_match.groups())
                hex_id = name_to_id[name]
                raw_monitors.append(Monitor(hex_id, name, is_primary, x, y, w, h))
                logging.debug(f"Matched Monitor {name} ({hex_id}) at {x},{y}")

        if not raw_monitors:
            logging.warning("No matching active monitors found.")
            return

        if not any(m.is_primary for m in raw_monitors):
            raw_monitors[0].is_primary = True

        pri = next(m for m in raw_monitors if m.is_primary)
        self.primary_id = pri.hex_id
        pri.slot = "primary"

        rights, lefts, aboves, belows = [], [], [], []
        for m in raw_monitors:
            if m.is_primary:
                continue
            if m.x > pri.x:
                rights.append(m)
            elif m.x < pri.x:
                lefts.append(m)
            elif m.y < pri.y:
                aboves.append(m)
            elif m.y > pri.y:
                belows.append(m)

        rights.sort(key=lambda m: m.x)
        lefts.sort(key=lambda m: m.x, reverse=True)
        aboves.sort(key=lambda m: m.y, reverse=True)
        belows.sort(key=lambda m: m.y)

        for idx, m in enumerate(rights, 1):
            m.slot = f"right_{idx}"
            m.prefix = ">" * idx

        for idx, m in enumerate(lefts, 1):
            m.slot = f"left_{idx}"
            m.prefix = "<" * idx

        for idx, m in enumerate(aboves, 1):
            m.slot = f"above_{idx}"
            m.prefix = "^" * idx

        for idx, m in enumerate(belows, 1):
            m.slot = f"below_{idx}"
            m.prefix = "v" * idx

        self.monitors = {m.hex_id: m for m in raw_monitors}
        for m_id, mon in self.monitors.items():
            logging.debug(f"Monitor {mon.name} ({m_id}) -> slot '{mon.slot}', prefix '{mon.prefix}'")

    def get_fallback_monitor_id(self, preferred_slot: str) -> str:
        """Finds the best available monitor Hex ID for a given spatial slot."""
        active_slots = {m.slot: m.hex_id for m in self.monitors.values()}

        if preferred_slot in active_slots:
            return active_slots[preferred_slot]

        # 1. Fallback in the same direction
        if "_" in preferred_slot:
            direction, idx = preferred_slot.split("_")
            idx = int(idx)
            for i in range(idx - 1, 0, -1):
                fallback = f"{direction}_{i}"
                if fallback in active_slots:
                    return active_slots[fallback]

        # 2. Fallback to any secondary
        secondaries = [m.hex_id for m in self.monitors.values() if not m.is_primary]
        if secondaries:
            return secondaries[0]

        # 3. Fallback to Primary
        return self.primary_id

    def sync_desktops(self):
            """Syncs all desktops across active monitors and drops stale IDs from memory."""
            active_bspwm_mon_ids = BspcClient.get_monitor_ids()
            valid_desktops = set()

            for m_id in active_bspwm_mon_ids:
                desk_ids = BspcClient.get_desktops(m_id)
                # Track all live desktops across all monitors before checking self.monitors
                # Prevents prematurely purging desktops pending evacuation
                valid_desktops.update(desk_ids)

                if m_id not in self.monitors:
                    continue

                for d_id in desk_ids:
                    if d_id not in self.desktops:
                        name = BspcClient.get_desktop_name(d_id)
                        self.desktops[d_id] = Desktop(d_id, name, m_id, self.monitors[m_id].slot)
                        logging.debug(f"Synced new desktop {d_id} on monitor {m_id}")

            stale_ids = [d_id for d_id in self.desktops if d_id not in valid_desktops]
            for d_id in stale_ids:
                del self.desktops[d_id]
                logging.debug(f"Purged stale desktop: {d_id}")

    def enforce_quotas(self):
        """Ensures minimum desktop counts exist on active monitors."""
        logging.debug("Enforcing quotas...")
        for m_id, mon in self.monitors.items():
            desks = BspcClient.get_desktops(m_id)
            minimum = 4 if mon.is_primary else 2

            if len(desks) < minimum:
                needed = minimum - len(desks)
                logging.info(f"Quota unmet on {mon.name} ({m_id}): adding {needed} desktops.")
                BspcClient.add_desktops(m_id, needed)

        self.sync_desktops()

    def route_desktops(self):
        """Evacuates disconnected monitors and restores desktops to preferred screens."""
        logging.debug("Routing desktops...")
        active_bspwm_mon_ids = BspcClient.get_monitor_ids()

        # Evacuate disconnected monitors by Hex ID
        for m_id in active_bspwm_mon_ids:
            if m_id not in self.monitors:
                logging.info(f"Evacuating detached monitor: {m_id}")
                desks = BspcClient.get_desktops(m_id)

                # Move all but the last desktop
                for d_id in desks[:-1]:
                    mem_slot = self.desktops.get(d_id, Desktop(d_id, "", "", "primary")).preferred_slot
                    fallback_id = self.get_fallback_monitor_id(mem_slot)
                    BspcClient.move_desktop(d_id, fallback_id)

                BspcClient.remove_monitor(m_id)

        # Restore desktops to preferred screens
        for d_id, desk in self.desktops.items():
            current_mon_id = BspcClient.run_cmd(["query", "--monitors", "--desktop", d_id])
            if not current_mon_id:
                continue

            target_mon_id = self.get_fallback_monitor_id(desk.preferred_slot)
            if current_mon_id != target_mon_id:
                BspcClient.move_desktop(d_id, target_mon_id)
                desk.current_monitor_id = target_mon_id

    def reorder_and_rename(self):
        """Sorts physical monitors and assigns desktop names strictly by monitor ID."""
        logging.debug("Reordering and renaming desktops...")
        ordered = sorted(self.monitors.values(), key=lambda m: (m.x, m.y))
        BspcClient.reorder_monitors([m.hex_id for m in ordered])

        global_idx = 1
        for mon in ordered:
            desks = BspcClient.get_desktops(mon.hex_id)
            local_idx = 1

            for d_id in desks:
                if mon.is_primary:
                    new_name = " " if local_idx <= 3 else str(global_idx)
                else:
                    if local_idx == 1:
                        new_name = f"{mon.prefix}{global_idx}"
                    elif local_idx in (2, 3):
                        new_name = " "
                    else:
                        new_name = f"{mon.prefix}{global_idx}"

                BspcClient.rename_desktop(d_id, new_name)
                if d_id in self.desktops:
                    self.desktops[d_id].name = new_name

                local_idx += 1
                global_idx += 1

    def handle_transfer(self, desktop_id: str, dest_monitor_id: str):
        """Directly processes a desktop transfer event via monitor Hex ID."""
        if dest_monitor_id in self.monitors:
            target_slot = self.monitors[dest_monitor_id].slot
            if desktop_id not in self.desktops:
                self.desktops[desktop_id] = Desktop(desktop_id, "", dest_monitor_id, target_slot)
            else:
                self.desktops[desktop_id].preferred_slot = target_slot
                self.desktops[desktop_id].current_monitor_id = dest_monitor_id

            logging.info(
                f"Manual Transfer: Desktop {desktop_id} -> {self.monitors[dest_monitor_id].name} ({dest_monitor_id}) [slot: {target_slot}]"
            )
            self.reorder_and_rename()

    def process_event_line(self, line: str) -> bool:
        """Parses a single subscription line. Returns True if a cycle should run."""
        parts = line.strip().split()
        if not parts:
            return False

        event = parts[0]
        logging.debug(f"Intercepted Event: {event}")

        if event in (
            "monitor_add",
            "monitor_remove",
            "monitor_geometry",
            "monitor_swap",
            "desktop_add",
            "desktop_remove",
            "desktop_swap",
        ):
            return True

        elif event == "desktop_transfer":
            # desktop_transfer <src_mon_id> <desktop_id> <dst_mon_id>
            if len(parts) >= 4:
                desk_id = parts[2]
                dst_mon_id = parts[3]
                self.handle_transfer(desk_id, dst_mon_id)
            return False

        return False

    def run_cycle(self):
        logging.debug("--- Starting Manager Cycle ---")
        self.parse_xrandr()
        self.sync_desktops()
        self.route_desktops()
        self.enforce_quotas()
        self.reorder_and_rename()
        logging.debug("--- Cycle Complete ---")


def listen_for_events(manager: WorkspaceManager):
    """Subscribes to BSPWM events and dispatches updates."""
    logging.info("Starting BSPWM event listener...")
    proc = subprocess.Popen(
        [
            "bspc", "subscribe",
            "monitor",
            "desktop_transfer", "desktop_swap",
            "desktop_add", "desktop_remove",
        ],
        stdout=subprocess.PIPE,
        text=True,
    )

    for line in proc.stdout:
        needs_cycle = manager.process_event_line(line)
        if needs_cycle:
            time.sleep(0.15)
            manager.run_cycle()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="BSPWM Monitor & Desktop Manager")
    parser.add_argument("--debug", action="store_true", help="Enable verbose debug logging")
    args = parser.parse_args()

    log_level = logging.DEBUG if args.debug else logging.WARNING
    logging.basicConfig(
        level=log_level,
        format="%(levelname)s: %(message)s",
        handlers=[logging.StreamHandler(sys.stderr)]
    )

    wm = WorkspaceManager()
    wm.run_cycle()
    listen_for_events(wm)

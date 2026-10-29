#!/usr/bin/env python3
"""
BSPWM Dynamic Monitor & Desktop Manager
Tracks and manipulates monitors and workspaces strictly via atomic bspc JSON dumps and Hex IDs.
"""

import argparse
from dataclasses import dataclass
import json
import logging
import re
import subprocess
import sys
import threading
import time
from typing import Dict, List, Optional


@dataclass
class Desktop:
    hex_id: str
    name: str
    current_monitor_id: str
    has_nodes: bool = False
    preferred_slot: str = "primary"


@dataclass
class Monitor:
    hex_id: str
    name: str
    is_primary: bool
    x: int
    y: int
    width: int
    height: int
    desktop_ids: List[str]
    slot: str = ""
    prefix: str = ""


class BspcClient:
    @staticmethod
    def run_cmd(args: List[str]) -> str:
        cmd_str = " ".join(["bspc"] + args)
        logging.debug(f"Exec: {cmd_str}")
        try:
            res = subprocess.run(
                ["bspc"] + args, capture_output=True, text=True, check=True
            )
            return res.stdout.rstrip("\n")
        except subprocess.CalledProcessError as e:
            err_msg = e.stderr.strip()
            if e.returncode == 1 and not err_msg:
                logging.debug(
                    f"Redundant command ignored seamlessly: {cmd_str}"
                )
                return ""
            logging.warning(f"bspc command failed: {cmd_str} - {err_msg}")
            return ""

    @staticmethod
    def get_wm_dump() -> dict:
        raw = BspcClient.run_cmd(["wm", "-d"])
        if not raw:
            return {}
        try:
            return json.loads(raw)
        except json.JSONDecodeError as e:
            logging.warning(f"Failed to parse wm -d output: {e}")
            return {}

    @staticmethod
    def move_desktop(desktop_id: str, monitor_id: str):
        BspcClient.run_cmd(["desktop", desktop_id, "--to-monitor", monitor_id])

    @staticmethod
    def rename_desktop(desktop_id: str, new_name: str):
        BspcClient.run_cmd(["desktop", desktop_id, "--rename", new_name])

    @staticmethod
    def add_desktops(monitor_id: str, count: int):
        if count > 0:
            names = [
                f"tmp_{monitor_id}_{i}_{int(time.time())}" for i in range(count)
            ]
            BspcClient.run_cmd(
                ["monitor", monitor_id, "--add-desktops"] + names
            )

    @staticmethod
    def remove_monitor(monitor_id: str):
        BspcClient.run_cmd(["monitor", monitor_id, "--remove"])

    @staticmethod
    def reorder_monitors(monitor_ids: List[str]):
        if monitor_ids:
            BspcClient.run_cmd(["wm", "--reorder-monitors"] + monitor_ids)


class WorkspaceManager:
    def __init__(self):
        self.monitors: Dict[str, Monitor] = {}
        self.desktops: Dict[str, Desktop] = {}
        self.primary_id: str = ""
        self.ignored_desktop_ids = set()
        self.programmatic_transfers = set()
        self.sync_in_progress = False

    def ingest_live_state(self):
        """Parses bspwm JSON state and xrandr layout atomically into unified dataclasses."""
        dump = BspcClient.get_wm_dump()
        if not dump:
            return

        xrandr_out = subprocess.run(
            ["xrandr", "--listactivemonitors"], capture_output=True, text=True
        ).stdout
        physical_monitors = {}
        for line in xrandr_out.splitlines():
            if not line.strip() or line.startswith("Monitors:"):
                continue
            parts = line.split()
            if len(parts) < 4:
                continue

            name = parts[-1]
            is_primary = "*" in parts[1]
            raw_geom = re.sub(r"/[0-9]+", "", parts[2])
            geom_match = re.search(
                r"(\d+)x(\d+)[+]?(-?\d+)[+]?(-?\d+)", raw_geom
            )

            if geom_match:
                w, h, x, y = map(int, geom_match.groups())
                physical_monitors[name] = (x, y, w, h, is_primary)

        if not physical_monitors:
            logging.warning(
                "xrandr reported 0 active monitors; skipping cycle to prevent monitor loss."
            )
            return

        if not any(m[4] for m in physical_monitors.values()):
            first_name = list(physical_monitors.keys())[0]
            physical_monitors[first_name] = (
                *physical_monitors[first_name][:4],
                True,
            )

        new_monitors: Dict[str, Monitor] = {}
        valid_desktop_ids = set()

        for m_data in dump.get("monitors", []):
            m_hex = f"0x{m_data['id']:08x}".lower()
            m_name = m_data.get("name", "")

            if m_name in physical_monitors:
                x, y, w, h, is_pri = physical_monitors[m_name]
            else:
                x, y, w, h, is_pri = 0, 0, 0, 0, False

            desk_ids = []
            for d_data in m_data.get("desktops", []):
                d_hex = f"0x{d_data['id']:08x}".lower()
                d_name = d_data.get("name", "")
                has_nodes = d_data.get("root") is not None

                desk_ids.append(d_hex)
                valid_desktop_ids.add(d_hex)

                if d_hex in self.desktops:
                    desk = self.desktops[d_hex]
                    desk.name = d_name
                    desk.current_monitor_id = m_hex
                    desk.has_nodes = has_nodes
                else:
                    self.desktops[d_hex] = Desktop(
                        d_hex, d_name, m_hex, has_nodes=has_nodes
                    )

            new_monitors[m_hex] = Monitor(
                m_hex, m_name, is_pri, x, y, w, h, desk_ids
            )

        self.monitors = new_monitors

        active_mons = [
            m for m in self.monitors.values() if m.name in physical_monitors
        ]
        if active_mons:
            pri = next((m for m in active_mons if m.is_primary), active_mons[0])
            self.primary_id = pri.hex_id
            pri.slot = "primary"

            rights, lefts, aboves, belows = [], [], [], []
            for m in active_mons:
                if m.hex_id == self.primary_id:
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
                m.slot, m.prefix = f"right_{idx}", ">" * idx
            for idx, m in enumerate(lefts, 1):
                m.slot, m.prefix = f"left_{idx}", "<" * idx
            for idx, m in enumerate(aboves, 1):
                m.slot, m.prefix = f"above_{idx}", "^" * idx
            for idx, m in enumerate(belows, 1):
                m.slot, m.prefix = f"below_{idx}", "v" * idx

        for d_hex, desk in self.desktops.items():
            if (
                desk.preferred_slot == "primary"
                and desk.current_monitor_id in self.monitors
            ):
                mon = self.monitors[desk.current_monitor_id]
                if mon.slot:
                    desk.preferred_slot = mon.slot

        stale_ids = [d for d in self.desktops if d not in valid_desktop_ids]
        for d in stale_ids:
            del self.desktops[d]
            logging.debug(f"Purged stale desktop: {d}")

    def get_fallback_monitor_id(self, preferred_slot: str) -> str:
        active_slots = {
            m.slot: m.hex_id for m in self.monitors.values() if m.slot
        }

        if preferred_slot in active_slots:
            return active_slots[preferred_slot]

        if "_" in preferred_slot:
            direction, idx = preferred_slot.split("_")
            for i in range(int(idx) - 1, 0, -1):
                fallback = f"{direction}_{i}"
                if fallback in active_slots:
                    return active_slots[fallback]

        secondaries = [
            m.hex_id
            for m in self.monitors.values()
            if m.slot and not m.is_primary
        ]
        if secondaries:
            return secondaries[0]

        return self.primary_id

    def route_desktops(self):
        """Evacuates dead monitors and restores orphans via memory routes."""
        logging.debug("Routing desktops...")
        detached = [m for m in self.monitors.values() if not m.slot]
        active = [m for m in self.monitors.values() if m.slot]
        mutated = False

        if detached:
            for m in detached:
                logging.info(f"Evacuating detached monitor: {m.hex_id}")
                if m.desktop_ids:
                    BspcClient.run_cmd(
                        [
                            "monitor",
                            m.hex_id,
                            "--add-desktops",
                            "__evac_dummy__",
                        ]
                    )
                    for d_id in m.desktop_ids:
                        desk = self.desktops.get(d_id)
                        fallback_id = self.get_fallback_monitor_id(
                            desk.preferred_slot if desk else "primary"
                        )
                        self.programmatic_transfers.add((d_id, fallback_id))
                        BspcClient.move_desktop(d_id, fallback_id)
                BspcClient.remove_monitor(m.hex_id)
            mutated = True

        for desk in self.desktops.values():
            if desk.current_monitor_id not in [m.hex_id for m in active]:
                continue

            target_mon_id = self.get_fallback_monitor_id(desk.preferred_slot)
            if desk.current_monitor_id != target_mon_id:
                self.programmatic_transfers.add((desk.hex_id, target_mon_id))
                BspcClient.move_desktop(desk.hex_id, target_mon_id)
                mutated = True

        if mutated:
            self.ingest_live_state()

        for m in self.monitors.values():
            if not m.slot or len(m.desktop_ids) <= 1:
                continue

            for d_id in list(m.desktop_ids):
                if len(m.desktop_ids) <= 1:
                    break
                desk = self.desktops.get(d_id)
                if (
                    desk
                    and not desk.has_nodes
                    and (
                        desk.name
                        in ("Desktop", "__evac_dummy__", "__plasma_sync__")
                        or desk.name.startswith("tmp_")
                    )
                ):
                    logging.info(
                        f"Pruning placeholder desktop {d_id} ('{desk.name}') from {m.hex_id}"
                    )
                    self.ignored_desktop_ids.add(d_id)
                    BspcClient.run_cmd(["desktop", d_id, "--remove"])
                    m.desktop_ids.remove(d_id)

    def enforce_quotas(self):
        """Ensures minimum desktop counts exist on active physical monitors."""
        logging.debug("Enforcing quotas...")
        mutated = False
        for m in self.monitors.values():
            if not m.slot:
                continue

            minimum = 4 if m.is_primary else 2
            if len(m.desktop_ids) < minimum:
                needed = minimum - len(m.desktop_ids)
                logging.info(
                    f"Quota unmet on {m.name} ({m.hex_id}): adding {needed} desktops."
                )
                BspcClient.add_desktops(m.hex_id, needed)
                mutated = True

        if mutated:
            self.ingest_live_state()

    def reorder_and_rename(self):
        """Sorts physical monitors and assigns standard naming conventions."""
        logging.debug("Reordering and renaming desktops...")
        active_ordered = sorted(
            [m for m in self.monitors.values() if m.slot],
            key=lambda m: (m.x, m.y),
        )
        if active_ordered:
            BspcClient.reorder_monitors([m.hex_id for m in active_ordered])

        global_idx = 1
        for mon in active_ordered:
            local_idx = 1
            for d_id in mon.desktop_ids:
                if mon.is_primary:
                    new_name = " " if local_idx <= 3 else str(global_idx)
                else:
                    if local_idx == 1:
                        new_name = f"{mon.prefix}{global_idx}"
                    elif local_idx in (2, 3):
                        new_name = " "
                    else:
                        new_name = f"{mon.prefix}{global_idx}"

                desk = self.desktops.get(d_id)
                if desk and desk.name != new_name:
                    BspcClient.rename_desktop(d_id, new_name)
                    desk.name = new_name

                local_idx += 1
                global_idx += 1

    def handle_transfer(self, desktop_id: str, dest_monitor_id: str):
        """Updates preferred slots when user explicitly moves a workspace."""
        if dest_monitor_id in self.monitors:
            target_slot = self.monitors[dest_monitor_id].slot
            if desktop_id in self.desktops:
                self.desktops[desktop_id].preferred_slot = target_slot
                logging.info(
                    f"Manual Transfer: Desktop {desktop_id} -> {self.monitors[dest_monitor_id].name} [slot: {target_slot}]"
                )

    def process_event_line(self, line: str) -> bool:
        parts = line.strip().split()
        if not parts:
            return False

        event = parts[0]
        logging.debug(f"Intercepted Event: {event}")

        if event == "desktop_add" and len(parts) >= 4:
            d_id = parts[2].lower()
            d_name = parts[3]
            if (
                self.sync_in_progress
                or d_name in ("__plasma_sync__", "__evac_dummy__")
                or d_name.startswith("tmp_")
            ):
                self.ignored_desktop_ids.add(d_id)
                return False
            return True

        if event == "desktop_remove" and len(parts) >= 3:
            d_id = parts[2].lower()
            if d_id in self.ignored_desktop_ids:
                self.ignored_desktop_ids.remove(d_id)
                return False
            return True

        if event in (
            "monitor_add",
            "monitor_remove",
            "monitor_geometry",
            "monitor_swap",
            "desktop_swap",
        ):
            return True

        if event == "desktop_transfer" and len(parts) >= 4:
            desk_id = parts[2].lower()
            dst_mon_id = parts[3].lower()

            transfer_key = (desk_id, dst_mon_id)
            if transfer_key in self.programmatic_transfers:
                self.programmatic_transfers.remove(transfer_key)
                return False

            self.handle_transfer(desk_id, dst_mon_id)
            return True

        return False

    def force_plasma_sync(self):
        """Adds a blank desktop and removes it by ID to trigger Plasma pager refresh."""
        if not self.primary_id:
            return

        logging.debug("Triggering Plasma UI sync workaround...")
        self.sync_in_progress = True
        try:
            BspcClient.run_cmd(
                ["monitor", self.primary_id, "--add-desktops", " "]
            )
            desks = BspcClient.run_cmd(
                ["query", "--desktops", "--monitor", self.primary_id]
            ).splitlines()

            if desks:
                new_id = desks[-1].lower()
                self.ignored_desktop_ids.add(new_id)
                time.sleep(0.05)
                BspcClient.run_cmd(["desktop", new_id, "--remove"])
        finally:
            self.sync_in_progress = False

    def run_cycle(self):
        logging.debug("--- Starting Manager Cycle ---")
        self.ingest_live_state()
        self.route_desktops()
        self.enforce_quotas()
        self.reorder_and_rename()
        self.force_plasma_sync()
        logging.debug("--- Cycle Complete ---")


class DebouncedRunner:
    def __init__(self, delay: float, target):
        self.delay = delay
        self.target = target
        self.timer: Optional[threading.Timer] = None
        self._timer_lock = threading.Lock()
        self._run_lock = threading.Lock()

    def trigger(self):
        with self._timer_lock:
            if self.timer is not None:
                self.timer.cancel()
            self.timer = threading.Timer(self.delay, self._execute)
            self.timer.daemon = True
            self.timer.start()

    def _execute(self):
        with self._run_lock:
            self.target()


def listen_for_events(manager: WorkspaceManager):
    logging.info("Starting BSPWM event listener...")
    proc = subprocess.Popen(
        [
            "bspc",
            "subscribe",
            "monitor",
            "desktop_transfer",
            "desktop_swap",
            "desktop_add",
            "desktop_remove",
        ],
        stdout=subprocess.PIPE,
        text=True,
    )

    debouncer = DebouncedRunner(delay=0.15, target=manager.run_cycle)

    for line in proc.stdout:
        if manager.process_event_line(line):
            debouncer.trigger()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="BSPWM Monitor & Desktop Manager"
    )
    parser.add_argument(
        "--debug", action="store_true", help="Enable verbose debug logging"
    )
    args = parser.parse_args()

    log_level = logging.DEBUG if args.debug else logging.WARNING
    logging.basicConfig(
        level=log_level,
        format="%(levelname)s: %(message)s",
        handlers=[logging.StreamHandler(sys.stderr)],
    )

    wm = WorkspaceManager()
    wm.run_cycle()
    listen_for_events(wm)

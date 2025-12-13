#!/usr/bin/env python3
# requirements: python3 usbutils gnugrep util-linux

import subprocess
import re
import sys


def run_lsusb_commands() -> tuple[str, str]:
    """Get USB device tree and listing output."""
    try:
        tree_result = subprocess.run(
            ["lsusb", "-t", "-v"], capture_output=True, text=True, check=True
        )
        lsusb_result = subprocess.run(
            ["lsusb"], capture_output=True, text=True, check=True
        )
        return tree_result.stdout, lsusb_result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to run lsusb: {e}")


def combine_tree_id_lines(tree_output: str) -> str:
    """
    Combine USB tree lines with their ID information.
    Lines starting with 'ID' are combined with the previous line.
    Normally, 'lsusb -t -v' outputs lines like:
        |__ Port 002: Dev 005, If 0, Class=Video, Driver=uvcvideo, 480M
        ID 046d:0843 Logitech, Inc. Webcam C930e
    We need to treat these as a single line, especially for indentation purposes
    """
    lines = tree_output.split("\n")
    combined_lines = []

    for i, line in enumerate(lines):
        if line.strip().startswith("ID ") and i > 0 and combined_lines:
            # Combine this ID line with the previous line
            combined_lines[-1] += " " + line.strip()
        else:
            combined_lines.append(line)

    return "\n".join(combined_lines)


def find_eyechip_device(lsusb_output: str) -> dict[str, str] | None:
    """Find eyechip/Tobii device in lsusb output."""
    for line in lsusb_output.split("\n"):
        if "Tobii" in line or "eyechip" in line.lower():
            match = re.search(
                r"Bus (\d+) Device (\d+): ID ([0-9a-f]{4}):([0-9a-f]{4})", line
            )
            if match:
                device_info = {
                    "bus": match.group(1),
                    "device_id": match.group(2),
                    "vendor": match.group(3),
                    "product": match.group(4),
                    "full_line": line.strip(),
                }
                print(f"Found eyechip device: {line.strip()}")
                return device_info
    return None


def find_hub_info(hub_dev_num: str, lsusb_output: str) -> dict[str, str | int] | None:
    """Find hub information from lsusb output by device number."""
    for line in lsusb_output.split("\n"):
        if f"Device {hub_dev_num}:" in line:
            match = re.search(
                r"Bus (\d+) Device (\d+): ID ([0-9a-f]{4}):([0-9a-f]{4}) (.+)", line
            )
            if match:
                bus_num, dev_num, vendor_id, product_id, name = match.groups()
                return {
                    "device_num": hub_dev_num,
                    "bus_device": f"{bus_num}/{dev_num}",
                    "vendor_product": f"{vendor_id}:{product_id}",
                    "name": name,
                    "full_line": line.strip(),
                    "indent_level": 0,  # Will be set later
                }
    return None


def get_indent_level(line: str) -> int:
    """Get the indentation level of a line (number of leading spaces)."""
    return len(line) - len(line.lstrip())


def find_eyechip_in_tree(eyechip_device: dict[str, str], tree_lines: list[str]) -> tuple[int, int] | None:
    """Find the eyechip device in USB tree and return (line_index, indent_level)."""
    device_pattern = f"ID {eyechip_device['vendor']}:{eyechip_device['product']}"

    for i, line in enumerate(tree_lines):
        if device_pattern in line and ("Tobii" in line or "eyechip" in line.lower()):
            indent_level = get_indent_level(line)
            print(f"Found eyechip in USB tree at line {i}, indent level: {indent_level}")
            print(line)
            return i, indent_level
    return None


def extract_hub_device_number(hub_line: str) -> str | None:
    """Extract device number from a hub line in USB tree."""
    hub_match = re.search(r"Dev (\d+)", hub_line)
    return hub_match.group(1) if hub_match else None


def find_parent_hubs(
    eyechip_device: dict[str, str], tree_output: str, lsusb_output: str
) -> list[dict[str, str | int]]:
    """Find all parent hubs of the eyechip device."""
    hub_hierarchy: list[dict[str, str | int]] = []

    # Combine tree lines with their ID information
    combined_tree_output = combine_tree_id_lines(tree_output)
    tree_lines = combined_tree_output.split("\n")

    # Find the eyechip device in the USB tree
    eyechip_location = find_eyechip_in_tree(eyechip_device, tree_lines)
    if not eyechip_location:
        print("ERROR: Eyechip device not found in USB tree")
        return hub_hierarchy

    eyechip_line_index, eyechip_indent = eyechip_location

    # Work backwards from eyechip to find parent hubs
    for i in range(eyechip_line_index - 1, -1, -1):
        line = tree_lines[i]

        # Skip empty lines
        if not line.strip():
            continue

        line_indent = get_indent_level(line)

        # Only consider lines with less indentation (higher in hierarchy) that are hubs
        if line_indent < eyechip_indent and "Class=Hub" in line:
            print(f"Found parent in USB tree at line {i}, indent level: {line_indent}")
            device_num = extract_hub_device_number(line)
            if device_num:
                hub_info = find_hub_info(device_num, lsusb_output)
                if hub_info:
                    hub_info["indent_level"] = line_indent
                    hub_hierarchy.append(hub_info)
                    print(f"Found parent hub: {hub_info['name']} (indent: {line_indent})")

    return hub_hierarchy


def find_root_hub(tree_output: str, lsusb_output: str) -> dict[str, str | int] | None:
    """Find the root hub as a fallback option."""
    combined_tree_output = combine_tree_id_lines(tree_output)
    tree_lines = combined_tree_output.split("\n")

    for line in tree_lines:
        if "Class=root_hub" in line:
            device_num = extract_hub_device_number(line)
            if device_num:
                hub_info = find_hub_info(device_num, lsusb_output)
                if hub_info:
                    hub_info["indent_level"] = get_indent_level(line)
                    print(f"Found root hub: {hub_info['name']}")
                    return hub_info
    return None


def select_hub_to_reset(
    hub_hierarchy: list[dict[str, str | int]],
) -> dict[str, str | int] | None:
    """Choose the most appropriate hub to reset."""
    if not hub_hierarchy:
        return None

    # Sort by indent level (lower indent = higher in hierarchy)
    hub_hierarchy.sort(key=lambda x: x["indent_level"])

    print(f"Found {len(hub_hierarchy)} parent hubs in hierarchy:")
    for i, hub in enumerate(hub_hierarchy):
        print(f"  Level {i}: {hub['name']}")

    if len(hub_hierarchy) == 1:
        print("Using only available parent hub")
        return hub_hierarchy[0]
    elif len(hub_hierarchy) >= 2:
        # Skip immediate parent, use next one up
        selected = hub_hierarchy[-2]
        print(f"Skipping immediate parent, using: {selected['name']}")
        return selected

    return None


def reset_usb_hub(hub: dict[str, str | int]) -> bool:
    """Reset the specified USB hub."""
    print(f"Resetting hub: {hub['name']}")
    print(f"Bus/Device: {hub['bus_device']}")

    try:
        result = subprocess.run(
            ["usbreset", str(hub["bus_device"])],
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

    eyechip_device = find_eyechip_device(lsusb_output)
    if not eyechip_device:
        print("ERROR: No eyechip/Tobii device found in USB devices")
        return False

    hub_hierarchy = find_parent_hubs(eyechip_device, tree_output, lsusb_output)
    if not hub_hierarchy:
        print("No parent hubs found for eyechip device, trying to find root hub as fallback...")
        root_hub = find_root_hub(tree_output, lsusb_output)
        if root_hub:
            print("Using root hub as fallback")
            return reset_usb_hub(root_hub)
        else:
            print("ERROR: No parent hubs or root hub found")
            return False

    selected_hub = select_hub_to_reset(hub_hierarchy)
    if not selected_hub:
        print("Could not determine suitable parent hub, trying root hub as fallback...")
        root_hub = find_root_hub(tree_output, lsusb_output)
        if root_hub:
            print("Using root hub as fallback")
            return reset_usb_hub(root_hub)
        else:
            print("ERROR: Could not determine suitable hub to reset")
            return False

    return reset_usb_hub(selected_hub)


if __name__ == "__main__":
    success = find_eyechip_and_reset_hub()
    sys.exit(0 if success else 1)

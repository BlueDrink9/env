#!/usr/bin/env bash
# rename_desktops.sh
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

declare -A MON_PREFIXES
DEFAULT_PREFIX="."
BLANK_DESKTOPS_COUNT=3

load_monitor_prefixes() {
    MON_PREFIXES=()
    local xrandr_out
    xrandr_out=$(xrandr --listactivemonitors 2>/dev/null || xrandr --listmonitors 2>/dev/null)

    # Direct call to parse_prefixes.awk
    while IFS=$'\t' read -r mon pfx; do
        [ -n "$mon" ] && MON_PREFIXES["$mon"]="$pfx"
    done < <(printf "%s\n" "$xrandr_out" | awk -f "$SCRIPT_DIR/parse_prefixes.awk")
}

rename_desktops() {
    load_monitor_prefixes

    local idx=0
    for desk in $(bspc query --desktops); do
        idx=$((idx + 1))

        if [ "$idx" -le "$BLANK_DESKTOPS_COUNT" ]; then
            bspc desktop "$desk" --rename " " 2>/dev/null || true
            continue
        fi

        local mon
        mon="$(bspc query --monitors --desktop "$desk" --names 2>/dev/null || true)"

        local prefix="${DEFAULT_PREFIX}"
        if [[ -v MON_PREFIXES["$mon"] ]]; then
            prefix="${MON_PREFIXES["$mon"]}"
        fi

        bspc desktop "$desk" --rename "${prefix}${idx}" 2>/dev/null || true
    done
}

if [ "${1:-}" = "--once" ]; then
    rename_desktops
    exit 0
fi

rename_desktops

bspc subscribe desktop_add desktop_remove desktop_swap desktop_transfer \
               monitor_add monitor_remove monitor_swap | while read -r _; do
    # Debounce rapid burst events
    while read -t 0.1 -r _; do :; done
    rename_desktops
done

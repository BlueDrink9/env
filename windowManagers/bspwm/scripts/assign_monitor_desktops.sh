#!/bin/sh

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
LOCKFILE="${XDG_RUNTIME_DIR:-/tmp}/bspwm_setup_monitors.lock"

MIN_PRIMARY_DESKTOPS=4
MIN_SECONDARY_DESKTOPS=2

# --- Display Hardware Management ---
toggle_internal_display() {
    local x_state
    x_state=$(xrandr)

    internal=$(echo "$x_state" | awk '/ connected/ && $1 ~ /^(eDP|LVDS|DSI)/ { print $1; exit }')
    ext_count=$(echo "$x_state" | awk '/ connected/ && $1 !~ /^(eDP|LVDS|DSI)/ { count++ } END { print count + 0 }')

    if [ "$ext_count" -gt 0 ] && [ -n "$internal" ]; then
        for ext in $(echo "$x_state" | awk '/ connected/ && $1 !~ /^(eDP|LVDS|DSI)/ { print $1 }'); do
            xrandr --output "$ext" --auto
        done
        xrandr --output "$internal" --off
    elif [ "$ext_count" -eq 0 ] && [ -n "$internal" ]; then
        xrandr --output "$internal" --auto --primary
    fi
}

order_screens_left_to_right() {
    if [ -n "${ORDERED_MONITORS:-}" ]; then
        bspc wm --reorder-monitors $ORDERED_MONITORS 2>/dev/null || true
    fi
}

# Evacuates desktops from disconnected displays to primary and cleans up monitors
reconcile_bspwm_monitors() {
    bsp_mons=$(bspc query --monitors --names 2>/dev/null || true)
    for bm in $bsp_mons; do
        is_active=0
        for am in $ACTIVE_MONITORS; do
            [ "$bm" = "$am" ] && is_active=1
        done

        if [ "$is_active" -eq 0 ]; then
            for desk in $(bspc query --desktops --monitor "$bm" 2>/dev/null); do
                bspc desktop "$desk" --to-monitor "$PRIMARY" 2>/dev/null || true
            done
            bspc monitor "$bm" --remove 2>/dev/null || true
        fi
    done
}

# Guarantees primary >= 4 desktops, secondary >= 2 desktops
# Does NOT remove desktops when counts exceed the minimum
ensure_desktop_quotas() {
    pri_desks=$(bspc query --desktops --monitor "$PRIMARY" 2>/dev/null || true)
    pri_count=$(printf "%s\n" "$pri_desks" | grep -c . || true)

    # 1. Satisfy secondary quota (>= 2) if it exists
    if [ -n "$SECONDARY" ]; then
        sec_desks=$(bspc query --desktops --monitor "$SECONDARY" 2>/dev/null || true)
        sec_count=$(printf "%s\n" "$sec_desks" | grep -c . || true)

        if [ "$sec_count" -lt "$MIN_SECONDARY_DESKTOPS" ]; then
            need_sec=$(( MIN_SECONDARY_DESKTOPS - sec_count ))
            spare_pri=$(( pri_count - MIN_PRIMARY_DESKTOPS ))
            [ "$spare_pri" -lt 0 ] && spare_pri=0

            transfer_count=$(( need_sec < spare_pri ? need_sec : spare_pri ))
            if [ "$transfer_count" -gt 0 ]; then
                trailing=$(printf "%s\n" "$pri_desks" | tail -n "$transfer_count")
                for d in $trailing; do
                    bspc desktop "$d" --to-monitor "$SECONDARY" 2>/dev/null || true
                done
                sec_count=$(( sec_count + transfer_count ))
                pri_count=$(( pri_count - transfer_count ))
            fi

            while [ "$sec_count" -lt "$MIN_SECONDARY_DESKTOPS" ]; do
                bspc monitor "$SECONDARY" --add-desktops "tmp_sec_$sec_count" 2>/dev/null || true
                sec_count=$(( sec_count + 1 ))
            done
        fi
    fi

    # 2. Satisfy primary quota (>= 4) - Runs for both Single & Dual setups
    while [ "$pri_count" -lt "$MIN_PRIMARY_DESKTOPS" ]; do
        bspc monitor "$PRIMARY" --add-desktops "tmp_pri_$pri_count" 2>/dev/null || true
        pri_count=$(( pri_count + 1 ))
    done
}

clean_orphaned_placeholders() {
    # Find all empty desktops matching "Desktop" or "tmp_*"
    for desk in $(bspc query --desktops); do
        desk_name=$(bspc query --desktops --desktop "$desk" --names)

        if [[ "$desk_name" == "Desktop" || "$desk_name" == tmp_* ]]; then
            # Check if it has zero windows
            if [ -z "$(bspc query --nodes --desktop "$desk" 2>/dev/null)" ]; then
                parent_mon=$(bspc query --monitors --desktop "$desk" --names 2>/dev/null || true)
                if [ -n "$parent_mon" ]; then
                    count=$(bspc query --desktops --monitor "$parent_mon" 2>/dev/null | grep -c . || true)
                    min_quota=$MIN_PRIMARY_DESKTOPS
                    [ "$parent_mon" = "$SECONDARY" ] && min_quota=$MIN_SECONDARY_DESKTOPS

                    if [ "$count" -gt "$min_quota" ]; then
                        bspc desktop "$desk" --remove 2>/dev/null || true
                    fi
                fi
            fi
        fi
    done
}

setup_monitors() {
    # Open lockfile on FD 9 and try to acquire an exclusive, non-blocking lock. Exit if locked.
    exec 9> "$LOCKFILE"
    flock -n 9 || return 0

    toggle_internal_display

    sleep 0.2

    eval "$(xrandr --listactivemonitors | awk -f "$SCRIPT_DIR/parse_monitors.awk")"
    [ -z "$PRIMARY" ] && { return 0; }

    # Normalize default monitor name if started clean
    if ! bspc query --monitors --names 2>/dev/null | grep -qx "$PRIMARY"; then
        if bspc query --monitors --names 2>/dev/null | grep -qx "default"; then
            bspc monitor default --rename "$PRIMARY" 2>/dev/null || true
        fi
    fi

    reconcile_bspwm_monitors
    ensure_desktop_quotas
    clean_orphaned_placeholders
    order_screens_left_to_right

    if [ -x "$SCRIPT_DIR/rename_desktops.sh" ]; then
        "$SCRIPT_DIR/rename_desktops.sh" --once
    fi
}

if [ -z "${TEST_MODE:-}" ]; then
    setup_monitors

    bspc subscribe monitor | while read -r event _id _name _geom; do
        case "$event" in
            monitor_add|monitor_remove)
                sleep 0.5
                setup_monitors
                ;;
        esac
    done
fi

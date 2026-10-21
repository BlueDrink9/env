#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

PASS=0
FAIL=0

assert_eq() {
    local desc="$1"
    local expected="$2"
    local actual="$3"
    if [ "$expected" = "$actual" ]; then
        printf "  [PASS] %s\n" "$desc"
        PASS=$((PASS + 1))
    else
        printf "  [FAIL] %s\n         Expected: '%s'\n         Got:      '%s'\n" "$desc" "$expected" "$actual"
        FAIL=$((FAIL + 1))
    fi
}

printf "=== 1. Monitor Parser Tests (parse_monitors.awk) ===\n"

REAL_INPUT="Monitors: 2
 0: +*DP-3-8 1920/527x1080/296+0+0  DP-3-8
 1: +DP-3-1 1920/509x1080/286+1920+0  DP-3-1"
eval "$(printf "%s\n" "$REAL_INPUT" | awk -f "$SCRIPT_DIR/parse_monitors.awk")"
assert_eq "Dual: Primary parsed" "DP-3-8" "$PRIMARY"
assert_eq "Dual: Primary geom cleaned" "1920x1080+0+0" "$PRIMARY_GEOM"
assert_eq "Dual: Secondary parsed" "DP-3-1" "$SECONDARY"
assert_eq "Dual: Secondary geom cleaned" "1920x1080+1920+0" "$SECONDARY_GEOM"
assert_eq "Dual: Active monitors" "DP-3-8 DP-3-1" "$ACTIVE_MONITORS"
assert_eq "Dual: Ordered monitors" "DP-3-8 DP-3-1" "$ORDERED_MONITORS"

INVERTED_INPUT="Monitors: 2
 0: +DP-3-8 1920/527x1080/296+0+0  DP-3-8
 1: +*DP-3-1 1920/509x1080/286+1920+0  DP-3-1"
eval "$(printf "%s\n" "$INVERTED_INPUT" | awk -f "$SCRIPT_DIR/parse_monitors.awk")"
assert_eq "Inverted: Primary parsed" "DP-3-1" "$PRIMARY"
assert_eq "Inverted: Secondary parsed" "DP-3-8" "$SECONDARY"
assert_eq "Inverted: Ordered monitors" "DP-3-8 DP-3-1" "$ORDERED_MONITORS"

VERTICAL_INPUT="Monitors: 2
 0: +*DP-1 1920/527x1080/296+0+1080  DP-1
 1: +DP-2 1920/509x1080/286+0+0  DP-2"
eval "$(printf "%s\n" "$VERTICAL_INPUT" | awk -f "$SCRIPT_DIR/parse_monitors.awk")"
assert_eq "Vertical: Primary name" "DP-1" "$PRIMARY"
assert_eq "Vertical: Secondary name" "DP-2" "$SECONDARY"
assert_eq "Vertical: Secondary Y is 0 (above)" "0" "$SECONDARY_Y"

FALLBACK_INPUT="Monitors: 1
 0: +eDP-1 1920/300x1200/200+0+0  eDP-1"
eval "$(printf "%s\n" "$FALLBACK_INPUT" | awk -f "$SCRIPT_DIR/parse_monitors.awk")"
assert_eq "Fallback: Primary set without asterisk" "eDP-1" "$PRIMARY"
assert_eq "Fallback: Secondary empty" "" "$SECONDARY"
assert_eq "Fallback: Ordered single monitor" "eDP-1" "$ORDERED_MONITORS"

printf "\n=== 2. Directional Prefix Tests (parse_prefixes.awk) ===\n"

declare -A PFX_MAP
while IFS=$'\t' read -r m p; do
    [ -n "$m" ] && PFX_MAP["$m"]="$p"
done < <(printf "%s\n" "$REAL_INPUT" | awk -f "$SCRIPT_DIR/parse_prefixes.awk")
assert_eq "Prefix: Primary monitor is empty" "" "${PFX_MAP["DP-3-8"]:-}"
assert_eq "Prefix: Secondary to the right is '>'" ">" "${PFX_MAP["DP-3-1"]:-}"

PFX_MAP=()
while IFS=$'\t' read -r m p; do
    [ -n "$m" ] && PFX_MAP["$m"]="$p"
done < <(printf "%s\n" "$INVERTED_INPUT" | awk -f "$SCRIPT_DIR/parse_prefixes.awk")
assert_eq "Prefix: Secondary to the left is '<'" "<" "${PFX_MAP["DP-3-8"]:-}"

PFX_MAP=()
while IFS=$'\t' read -r m p; do
    [ -n "$m" ] && PFX_MAP["$m"]="$p"
done < <(printf "%s\n" "$VERTICAL_INPUT" | awk -f "$SCRIPT_DIR/parse_prefixes.awk")
assert_eq "Prefix: Secondary physically above is '^'" "^" "${PFX_MAP["DP-2"]:-}"

printf "\n=== 3. Desktop Quota Allocation Simulation ===\n"

simulate_quotas() {
    local has_sec="$1"
    local pri_cnt="$2"
    local sec_cnt="$3"
    local transferred=0
    local added_pri=0
    local added_sec=0

    if [ "$has_sec" = "1" ]; then
        if [ "$sec_cnt" -lt 2 ]; then
            local need_sec=$(( 2 - sec_cnt ))
            local spare_pri=$(( pri_cnt - 4 ))
            [ "$spare_pri" -lt 0 ] && spare_pri=0
            transferred=$(( need_sec < spare_pri ? need_sec : spare_pri ))
            sec_cnt=$(( sec_cnt + transferred ))
            pri_cnt=$(( pri_cnt - transferred ))
            need_sec=$(( 2 - sec_cnt ))
            added_sec=$need_sec
            sec_cnt=$(( sec_cnt + added_sec ))
        fi
        if [ "$pri_cnt" -lt 4 ]; then
            added_pri=$(( 4 - pri_cnt ))
            pri_cnt=$(( pri_cnt + added_pri ))
        fi
    else
        if [ "$pri_cnt" -lt 4 ]; then
            added_pri=$(( 4 - pri_cnt ))
            pri_cnt=$(( pri_cnt + added_pri ))
        fi
    fi
    printf "TRANS=%d ADD_PRI=%d ADD_SEC=%d FINAL_PRI=%d FINAL_SEC=%d\n" \
        "$transferred" "$added_pri" "$added_sec" "$pri_cnt" "$sec_cnt"
}

# Boot dual: 1 desktop on primary, 1 on secondary
eval "$(simulate_quotas 1 1 1)"
assert_eq "Boot dual: Added to pri" "3" "$ADD_PRI"
assert_eq "Boot dual: Added to sec" "1" "$ADD_SEC"
assert_eq "Boot dual: Final pri count" "4" "$FINAL_PRI"
assert_eq "Boot dual: Final sec count" "2" "$FINAL_SEC"

# Plug secondary in: 6 desktops exist on primary, 0 on secondary
eval "$(simulate_quotas 1 6 0)"
assert_eq "Plug secondary: Transferred from pri" "2" "$TRANS"
assert_eq "Plug secondary: Added to pri" "0" "$ADD_PRI"
assert_eq "Plug secondary: Added to sec" "0" "$ADD_SEC"
assert_eq "Plug secondary: Final pri count" "4" "$FINAL_PRI"
assert_eq "Plug secondary: Final sec count" "2" "$FINAL_SEC"

# Reload dual: 5 on primary, 3 on secondary (preserves excess)
eval "$(simulate_quotas 1 5 3)"
assert_eq "Reload excess dual: None transferred" "0" "$TRANS"
assert_eq "Reload excess dual: None added to pri" "0" "$ADD_PRI"
assert_eq "Reload excess dual: None added to sec" "0" "$ADD_SEC"
assert_eq "Reload excess dual: Primary preserved" "5" "$FINAL_PRI"
assert_eq "Reload excess dual: Secondary preserved" "3" "$FINAL_SEC"

# Boot single monitor: 1 desktop
eval "$(simulate_quotas 0 1 0)"
assert_eq "Boot single: Added to pri" "3" "$ADD_PRI"
assert_eq "Boot single: Final pri count" "4" "$FINAL_PRI"

# Reload single monitor: 6 desktops (preserves excess)
eval "$(simulate_quotas 0 6 0)"
assert_eq "Reload single: None added" "0" "$ADD_PRI"
assert_eq "Reload single: Final pri count preserved" "6" "$FINAL_PRI"

printf "\nTest Results: %d Passed, %d Failed\n" "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]

#!/usr/bin/awk -f

NR > 1 {
    # Replace anything outside standard printable ASCII (including non-breaking spaces) with a space
    gsub(/[^ -~]/, " ")

    # Match monitor listing lines (e.g., ' 0: +*DP-1 ...' or '1: +DP-2 ...')
    if ($0 !~ /[0-9]+:[ \t]*\+/) next

    mon = $NF
    is_pri = ($0 ~ /[0-9]+:[ \t]*\+\*/) ? 1 : 0
    raw_geom = $3
    gsub(/\/[0-9]+/, "", raw_geom)

    split(raw_geom, parts, "+")
    geom_wh = parts[1]
    x_pos = parts[2] + 0
    y_pos = parts[3] + 0

    count++
    mons[count] = mon
    pris[count] = is_pri
    geoms[count] = raw_geom
    xs[count] = x_pos
    ys[count] = y_pos

    if (is_pri) {
        pri_idx = count
    }
}

END {
    # Fallback: if no monitor is flagged primary (*), select the first one
    if (pri_idx == 0 && count > 0) {
        pri_idx = 1
        pris[1] = 1
    }

    pri_name = (pri_idx > 0) ? mons[pri_idx] : ""
    pri_geom = (pri_idx > 0) ? geoms[pri_idx] : ""
    pri_x = (pri_idx > 0) ? xs[pri_idx] : 0
    pri_y = (pri_idx > 0) ? ys[pri_idx] : 0

    sec_idx = 0
    for (i = 1; i <= count; i++) {
        if (i != pri_idx) {
            sec_idx = i
            break
        }
    }

    sec_name = (sec_idx > 0) ? mons[sec_idx] : ""
    sec_geom = (sec_idx > 0) ? geoms[sec_idx] : ""
    sec_x = (sec_idx > 0) ? xs[sec_idx] : 0
    sec_y = (sec_idx > 0) ? ys[sec_idx] : 0

    printf "PRIMARY=\"%s\"; PRIMARY_GEOM=\"%s\"; PRIMARY_X=%d; PRIMARY_Y=%d;\n", pri_name, pri_geom, pri_x, pri_y
    printf "SECONDARY=\"%s\"; SECONDARY_GEOM=\"%s\"; SECONDARY_X=%d; SECONDARY_Y=%d;\n", sec_name, sec_geom, sec_x, sec_y

    all_mons = ""
    for (i = 1; i <= count; i++) {
        all_mons = (i == 1) ? mons[i] : (all_mons " " mons[i])
    }
    printf "ACTIVE_MONITORS=\"%s\";\n", all_mons

    # Sort left-to-right by X offset for bspc wm --reorder-monitors
    for (i = 1; i <= count; i++) {
        for (j = i + 1; j <= count; j++) {
            if (xs[i] > xs[j]) {
                tmp_m = mons[i]; mons[i] = mons[j]; mons[j] = tmp_m;
                tmp_x = xs[i]; xs[i] = xs[j]; xs[j] = tmp_x;
            }
        }
    }

    ord_mons = ""
    for (i = 1; i <= count; i++) {
        ord_mons = (i == 1) ? mons[i] : (ord_mons " " mons[i])
    }
    printf "ORDERED_MONITORS=\"%s\";\n", ord_mons
}

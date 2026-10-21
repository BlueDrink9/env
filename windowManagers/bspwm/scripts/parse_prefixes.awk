#!/usr/bin/awk -f

NR > 1 {
    # Replace anything outside standard printable ASCII (including non-breaking spaces) with a space
    gsub(/[^ -~]/, " ")

    if ($0 !~ /[0-9]+:[ \t]*\+/) next

    mon = $NF
    is_pri = ($0 ~ /[0-9]+:[ \t]*\+\*/) ? 1 : 0
    raw_geom = $3
    gsub(/\/[0-9]+/, "", raw_geom)

    split(raw_geom, parts, "+")
    x = parts[2] + 0
    y = parts[3] + 0

    count++
    mons[count] = mon
    xs[count] = x
    ys[count] = y

    if (is_pri) {
        pri_idx = count
    }
}

END {
    if (pri_idx == 0 && count > 0) {
        pri_idx = 1
    }

    pri_name = (pri_idx > 0) ? mons[pri_idx] : ""
    pri_x = (pri_idx > 0) ? xs[pri_idx] : 0
    pri_y = (pri_idx > 0) ? ys[pri_idx] : 0

    for (i = 1; i <= count; i++) {
        m = mons[i]
        if (m == pri_name) {
            pfx = ""
        } else {
            h = (xs[i] < pri_x) ? "<" : ((xs[i] > pri_x) ? ">" : "")
            # Screen coordinates: smaller Y means physically above
            v = (ys[i] < pri_y) ? "^" : ((ys[i] > pri_y) ? "v" : "")
            pfx = h v
        }
        printf "%s\t%s\n", m, pfx
    }
}

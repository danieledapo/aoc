#!/usr/bin/env python3

import re
import sys

machines = sys.stdin.read().strip().split("\n\n")

res = [0, 0]

for machine in machines:
    ax, ay, bx, by, pox, poy = [int(n) for n in re.findall(r"\d+", machine)]

    for i, d in enumerate((0, 10000000000000)):
        px, py = pox + d, poy + d

        # ax*a + bx*b = px
        # ay*a + by*b = py
        ca = (px * by - py * bx) / (ax * by - ay * bx)
        cb = (px - ax * ca) / bx
        if int(ca) == ca and int(cb) == cb:
            res[i] += int(ca * 3 + cb)

print(*res, sep="\n")

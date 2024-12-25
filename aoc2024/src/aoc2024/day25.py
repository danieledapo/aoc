#!/usr/bin/env python3

import sys

inp = sys.stdin.read().strip()

locks = []
keys = []

for g in inp.split("\n\n"):
    g = g.splitlines()

    heights = []
    for c in range(len(g[0])):
        height = sum(1 for r in range(len(g)) if g[r][c] == "#")
        heights.append(height - 1)

    if g[0].startswith("#"):
        locks.append(heights)
    else:
        keys.append(heights)

print(
    sum(
        1
        for lock in locks
        for key in keys
        if all(l + k <= 5 for l, k in zip(lock, key))
    )
)

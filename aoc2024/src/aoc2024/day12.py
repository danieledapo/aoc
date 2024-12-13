#!/usr/bin/env python3

import sys

from collections import defaultdict

DIRS = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def calc_region(grid, r, c):
    if grid[r][c] is None:
        return 0, 0

    plant = grid[r][c]
    plants = set()
    to_visit = [(r, c)]
    while to_visit:
        rr, cc = to_visit.pop()
        if (rr, cc) in plants or grid[rr][cc] != plant:
            continue

        grid[rr][cc] = None
        plants.add((rr, cc))

        to_visit.extend(
            (rr + dr, cc + dc)
            for dr, dc in DIRS
            if 0 <= rr + dr < len(grid) and 0 <= cc + dc < len(grid[0])
        )

    area = len(plants)

    sides = [defaultdict(list), defaultdict(list), defaultdict(list), defaultdict(list)]
    perimeter = 0
    for r, c in plants:
        for side, (dr, dc) in enumerate(DIRS):
            if (r + dr, c + dc) in plants:
                continue

            perimeter += 1

            if dr != 0:
                sides[side][r].append(c)
            else:
                sides[side][c].append(r)

    nsides = 0
    for side in sides:
        for arr in side.values():
            cur = None
            for v in sorted(arr):
                if cur is not None and cur + 1 != v:
                    nsides += 1
                cur = v
            nsides += 1

    return area * perimeter, area * nsides


grid = [list(l) for l in sys.stdin.read().strip().splitlines()]

p1 = 0
p2 = 0
for r in range(len(grid)):
    for c in range(len(grid[r])):
        a, b = calc_region(grid, r, c)
        p1 += a
        p2 += b

print(p1)
print(p2)

#!/usr/bin/env python3

import sys

from itertools import combinations


def calc_dists(grid):
    width, height = len(grid[0]), len(grid)
    dists = {}

    to_visit = []
    for r in range(height):
        for c in range(width):
            if grid[r][c] == "S":
                to_visit.append((0, r, c))

    while to_visit:
        cost, r, c = to_visit.pop()
        if (r, c) in dists:
            continue

        dists[(r, c)] = cost

        for rr, cc in [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]:
            if 0 <= rr < height and 0 <= cc < width and grid[rr][cc] != "#":
                to_visit.append((cost + 1, rr, cc))

    return dists


grid = [list(l.strip()) for l in sys.stdin.readlines()]
dists = calc_dists(grid)

p1 = 0
p2 = 0
for ((r0, c0), d0), ((r1, c1), d1) in combinations(dists.items(), 2):
    dst = abs(r0 - r1) + abs(c0 - c1)
    if dst <= 2 and d1 - d0 - dst >= 100:
        p1 += 1
    if dst <= 20 and d1 - d0 - dst >= 100:
        p2 += 1

print(p1)
print(p2)

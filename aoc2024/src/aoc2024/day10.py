#!/usr/bin/env python3

import sys


def trails(grid, r, c):
    rows, cols = len(grid), len(grid[0])

    lev = grid[r][c]
    if lev == 9:
        return [[(r, c)]]

    tt = []
    for dr, dc in ((-1, 0), (1, 0), (0, 1), (0, -1)):
        rr, cc = r + dr, c + dc
        if 0 <= rr < rows and 0 <= cc < cols and grid[rr][cc] - 1 == lev:
            for t in trails(grid, rr, cc):
                tt.append([(r, c)] + t)

    return tt


grid = [[int(d) for d in l.strip()] for l in sys.stdin.readlines()]

print(
    sum(
        len({t[-1] for t in trails(grid, r, c)})
        for r in range(len(grid))
        for c in range(len(grid[r]))
        if grid[r][c] == 0
    )
)

print(
    sum(
        len(trails(grid, r, c))
        for r in range(len(grid))
        for c in range(len(grid[r]))
        if grid[r][c] == 0
    )
)

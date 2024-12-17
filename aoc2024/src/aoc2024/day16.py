#!/usr/bin/env python3

import sys
from heapq import heappop, heappush


def run(maze):
    width, height = len(maze[0]), len(maze)

    r, c = -1, -1
    for row in range(height):
        for col in range(width):
            if maze[row][col] == "S":
                r, c = row, col

    seen = {}
    end_cost = float("inf")

    to_visit = [(0, r, c, 0, 1, [(r, c)])]
    while to_visit:
        cost, r, c, dr, dc, path = heappop(to_visit)

        if seen.setdefault((r, c, dr, dc), cost) > end_cost:
            continue

        if maze[r][c] == "E":
            end_cost = cost
            yield cost, path
            continue

        for move_cost, dr, dc in [(1, dr, dc), (1001, -dc, dr), (1001, dc, -dr)]:
            if cost + move_cost > end_cost:
                continue

            if not (0 <= r + dr < height and 0 <= c + dc < width):
                continue

            if maze[r + dr][c + dc] == "#":
                continue

            if seen.get((r + dr, c + dc, dr, dc), float("inf")) < cost + move_cost:
                continue

            heappush(
                to_visit,
                (
                    cost + move_cost,
                    r + dr,
                    c + dc,
                    dr,
                    dc,
                    path + [(r + dr, c + dc)],
                ),
            )


maze = [l.strip() for l in sys.stdin.read().strip().splitlines()]

p2 = set()
for cost, path in run(maze):
    if not p2:
        print(cost)
    p2 |= set(path)
print(len(p2))

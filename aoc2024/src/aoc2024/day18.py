#!/usr/bin/env python3

import sys

from heapq import heappop, heappush


def find_path(maze):
    seen = set()
    to_visit = [(0, 0, 0)]

    while to_visit:
        cost, r, c = heappop(to_visit)

        if (r, c) in seen:
            continue
        seen.add((r, c))

        if (r, c) == (size - 1, size - 1):
            return cost

        for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            rr, cc = r + dr, c + dc
            if (
                0 <= rr < size
                and 0 <= cc < size
                and maze[rr][cc] != "#"
                and (rr, cc) not in seen
            ):
                heappush(to_visit, (cost + 1, rr, cc))

    return None


bytes = [tuple(map(int, l.strip().split(","))) for l in sys.stdin.read().splitlines()]


size = 71
maze = [["." for _ in range(size)] for _ in range(size)]
for x, y in bytes[:1024]:
    maze[y][x] = "#"
print(find_path(maze))

maze = [["." for _ in range(size)] for _ in range(size)]
for x, y in bytes:
    maze[y][x] = "#"
    if find_path(maze) is None:
        print(f"{x},{y}")
        break

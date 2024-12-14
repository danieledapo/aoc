#!/usr/bin/env python3

import sys
import re

GRID_WIDTH = 101
GRID_HEIGHT = 103


def move_robot(px, py, vx, vy):
    px = (px + vx) % GRID_WIDTH
    py = (py + vy) % GRID_HEIGHT
    return (px, py, vx, vy)


def part1(robots):
    for _ in range(100):
        robots = [move_robot(*r) for r in robots]

    q0, q1, q2, q3 = 0, 0, 0, 0
    for px, py, _, _ in robots:
        mx = GRID_WIDTH // 2
        my = GRID_HEIGHT // 2

        if px < mx and py < my:
            q0 += 1
        elif px < mx and py > my:
            q1 += 1
        elif px > mx and py < my:
            q2 += 1
        elif px > mx and py > my:
            q3 += 1

    return q0 * q1 * q2 * q3


def part2(robots, max_iters=10000):
    best = (0, [], -1)
    for t in range(1, max_iters + 1):
        robots = [move_robot(*r) for r in robots]

        robot_pts = {(x, y) for x, y, _, _ in robots}
        added = set()
        areas = []
        for px, py, _, _ in robots:
            if (px, py) in added:
                continue

            areas.append([])
            to_visit = [(px, py)]
            while to_visit:
                px, py = to_visit.pop()

                if (px, py) in added:
                    continue
                added.add((px, py))

                areas[-1].append((px, py))
                for xx, yy in [(px - 1, py), (px + 1, py), (px, py - 1), (px, py + 1)]:
                    if (xx, yy) in robot_pts:
                        to_visit.append((xx, yy))

        biggest = max(areas, key=len)

        if best is None or best[0] < len(biggest):
            # print(t, len(biggest))
            best = (len(biggest), robots[:], t)
    return best


robots = [
    tuple(map(int, re.findall(r"-?\d+", l.strip())))
    for l in sys.stdin.read().splitlines()
]

print(part1(robots[:]))

_, robots, t = part2(robots[:])
print(t)

grid = [[" " for _ in range(GRID_WIDTH)] for _ in range(GRID_HEIGHT)]
for px, py, _, _ in robots:
    grid[py][px] = "o"

print("\n".join(("".join(row) for row in grid)))

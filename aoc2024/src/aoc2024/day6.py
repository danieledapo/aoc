#!/usr/bin/env python

from collections import defaultdict
import copy
from multiprocessing import Pool
import sys


def run(pos, lines):
    seen = defaultdict(set)
    dir = (0, -1)
    while True:
        if dir in seen.get(pos, set()):
            return

        seen[pos].add(dir)
        nx, ny = pos[0] + dir[0], pos[1] + dir[1]
        if not (0 <= nx < len(lines[0]) and 0 <= ny < len(lines)):
            break

        if lines[ny][nx] == "#":
            dir = -dir[1], dir[0]
            continue

        pos = nx, ny
    return seen


lines = [list(l) for l in sys.stdin.read().strip().splitlines()]

pos = None
for row, line in enumerate(lines):
    for col, c in enumerate(line):
        if c == "^":
            pos = (col, row)
assert pos is not None


seen = run(pos, lines)
print(len(seen))


def part2(opos):
    c, r = opos
    if lines[r][c] != ".":
        return 0

    new_lines = copy.deepcopy(lines)
    new_lines[r][c] = "#"
    if run(pos, new_lines) is None:
        return 1

    return 0


with Pool() as pool:
    p2 = sum(pool.map(part2, seen))
print(p2)

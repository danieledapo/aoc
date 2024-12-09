#!/usr/bin/env python3

import copy
import itertools
import sys

from collections import defaultdict


def calc_antinodes(antennas, offsets):
    antinodes = set()

    for ants in antennas.values():
        for i in range(len(ants)):
            for j in range(i + 1, len(ants)):
                r1, c1 = ants[i]
                r2, c2 = ants[j]

                for z in copy.deepcopy(offsets):
                    r3, c3 = r1 + z * (r1 - r2), c1 + z * (c1 - c2)
                    if 0 <= r3 < rows and 0 <= c3 < cols:
                        antinodes.add((r3, c3))
                    else:
                        break

                for z in copy.deepcopy(offsets):
                    r4, c4 = r2 + z * (r2 - r1), c2 + z * (c2 - c1)
                    if 0 <= r4 < rows and 0 <= c4 < cols:
                        antinodes.add((r4, c4))
                    else:
                        break

    return len(antinodes)


grid = [l.strip() for l in sys.stdin.readlines()]
rows, cols = len(grid), len(grid[0])

antennas = defaultdict(list)
for row, line in enumerate(grid):
    for col, c in enumerate(line):
        if c == ".":
            continue
        antennas[c].append((col, row))


print(calc_antinodes(antennas, offsets=[1]))
print(calc_antinodes(antennas, offsets=itertools.count()))

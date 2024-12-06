#!/usr/bin/env python3

import sys


lines = [l for l in sys.stdin.readlines() if l]

p1 = 0
p2 = 0

for r, line in enumerate(lines):
    for c in range(len(line)):
        for dr in (0, 1, -1):
            for dc in (0, 1, -1):
                if not (0 <= r + dr * 3 < len(lines) and 0 <= c + dc * 3 < len(line)):
                    continue

                word = "".join(lines[r + dr * i][c + dc * i] for i in range(4))
                if word == "XMAS":
                    p1 += 1

        if (
            1 <= r < len(lines) - 1
            and 1 <= c < len(line) - 1
            and all(
                lines[r - 1][c - dc] + lines[r][c] + lines[r + 1][c + dc]
                in ("MAS", "SAM")
                for dc in (1, -1)
            )
        ):
            p2 += 1


print(p1)
print(p2)

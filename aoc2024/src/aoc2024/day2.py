#!/usr/bin/env python3

import sys


def is_safe(levels):
    diff = 0

    for prev, cur in zip(levels, levels[1:]):
        d = cur - prev

        if abs(d) < 1 or abs(d) > 3:
            return False

        if d * diff < 0:
            return False

        diff = d

    return True


reports = [[int(lev) for lev in line.split()] for line in sys.stdin.readlines()]

print(sum((1 for report in reports if is_safe(report))))

p2 = 0
for report in reports:
    for i in range(len(report)):
        rep = report[:i] + report[i + 1 :]
        if is_safe(rep):
            p2 += 1
            break

print(p2)

#!/usr/bin/env python3

import sys


def arrangements(towels, design, cache):
    if design == "":
        return 1

    if design in cache:
        return cache[design]

    res = 0
    for towel in towels:
        if design.startswith(towel):
            res += arrangements(towels, design[len(towel) :], cache)

    cache[design] = res
    return res


towels, _, *designs = [l.strip() for l in sys.stdin.readlines()]
towels = towels.split(", ")

cache = {}

print(sum(1 for design in designs if arrangements(towels, design, cache) > 0))
print(sum(arrangements(towels, design, cache) for design in designs))

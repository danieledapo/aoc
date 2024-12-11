#!/usr/bin/env python3

import sys


def generates(stone: int, turns: int, cache: dict[tuple[int, int], int]) -> int:
    if (stone, turns) in cache:
        return cache[(stone, turns)]

    if turns <= 0:
        return 1

    stone_str = str(stone)

    res = 0
    if stone == 0:
        res = generates(1, turns - 1, cache)
    elif len(stone_str) % 2 == 0:
        mid = len(stone_str) // 2
        res += generates(int(stone_str[:mid]), turns - 1, cache)
        res += generates(int(stone_str[mid:]), turns - 1, cache)
    else:
        res = generates(stone * 2024, turns - 1, cache)

    cache[(stone, turns)] = res
    return res


stones = [int(n) for n in sys.stdin.read().strip().split()]
cache = {}
print(sum(generates(stone, 25, cache) for stone in stones))
print(sum(generates(stone, 75, cache) for stone in stones))

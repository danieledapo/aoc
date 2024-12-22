#!/usr/bin/env python3

import sys


NUMERIC_KEYPAD = (
    "789",
    "456",
    "123",
    " 0A",
)

DIRECTIONAL_KEYPAD = (
    " ^A",
    "<v>",
)


def path(keypad, src, dst):
    sx, sy = 0, 0
    dx, dy = 0, 0
    for y, line in enumerate(keypad):
        pos = line.find(src)
        if pos >= 0:
            sx, sy = (pos, y)

        pos = line.find(dst)
        if pos >= 0:
            dx, dy = (pos, y)

    def find_paths(x, y, s):
        if (x, y) == (dx, dy):
            yield s + "A"
        if dx < x and keypad[y][x - 1] != " ":
            yield from find_paths(x - 1, y, s + "<")
        if dy < y and keypad[y - 1][x] != " ":
            yield from find_paths(x, y - 1, s + "^")
        if dy > y and keypad[y + 1][x] != " ":
            yield from find_paths(x, y + 1, s + "v")
        if dx > x and keypad[y][x + 1] != " ":
            yield from find_paths(x + 1, y, s + ">")

    return min(
        find_paths(sx, sy, ""), key=lambda p: sum(a != b for a, b in zip(p, p[1:]))
    )


def solve(code, depth, cache, max_depth=2):
    if depth > max_depth:
        return len(code)

    if (max_depth, code, depth) in cache:
        return cache[(max_depth, code, depth)]

    ret = sum(
        solve(
            path(DIRECTIONAL_KEYPAD if depth > 0 else NUMERIC_KEYPAD, src, dst),
            depth + 1,
            cache,
            max_depth=max_depth,
        )
        for src, dst in zip("A" + code, code)
    )
    cache[(max_depth, code, depth)] = ret
    return ret


codes = [l.strip() for l in sys.stdin.readlines()]
cache = {}

p1 = 0
p2 = 0
for code in codes:
    icode = int(code.lstrip("0").rstrip("A"))

    p1 += solve(code, 0, cache) * icode
    p2 += solve(code, 0, cache, max_depth=25) * icode

print(p1)
print(p2)

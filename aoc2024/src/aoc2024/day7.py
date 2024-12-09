#!/usr/bin/env python3

import sys


def solve(target: int, operands: list[int], concat: bool = False) -> bool:
    if len(operands) <= 1:
        return operands == [target]

    a, b = operands[0], operands[1]
    if a > target:
        return False

    if solve(target, [a + b] + operands[2:], concat=concat):
        return True

    if solve(target, [a * b] + operands[2:], concat=concat):
        return True

    if concat and solve(target, [int(str(a) + str(b))] + operands[2:], concat=concat):
        return True

    return False


program = []

for l in sys.stdin.readlines():
    target, operands = l.split(": ")
    program.append((int(target), [int(o) for o in operands.split()]))

p1 = sum(target for target, operands in program if solve(target, operands))
print(p1)

p2 = sum(target for target, operands in program if solve(target, operands, concat=True))
print(p2)

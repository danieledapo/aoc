#!/usr/bin/env python3

import re
import sys

program = sys.stdin.read()

cond_mul_re = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)|(do\(\)|don't\(\))")

p1 = 0
p2 = 0
enabled = True

for a, b, c in cond_mul_re.findall(program):
    if c:
        enabled = c == "do()"
    else:
        p1 += int(a) * int(b)
        if enabled:
            p2 += int(a) * int(b)

print(p1)
print(p2)

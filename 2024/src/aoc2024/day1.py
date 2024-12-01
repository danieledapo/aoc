#!/usr/bin/env python3

import bisect
import sys

left = []
right = []


for line in sys.stdin.readlines():
    l, r = line.split()
    left.append(int(l))
    right.append(int(r))

left.sort()
right.sort()

print(sum((abs(r - l) for l, r in zip(left, right))))

p2 = 0
for l in left:
    i = bisect.bisect_left(right, l)
    while i < len(right) and right[i] == l:
        p2 += l
        i += 1

print(p2)

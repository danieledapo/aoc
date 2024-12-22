#!/usr/bin/env python3

import sys

from collections import defaultdict


def calc(n, iters=2000):
    for _ in range(iters):
        n = (n ^ (n * 64)) % 16777216
        n = (n ^ (n // 32)) % 16777216
        n = (n ^ (n * 2048)) % 16777216
        yield n


ns = [int(l.strip()) for l in sys.stdin.readlines()]
monkeys = [list(calc(n)) for n in ns]

price_changes = defaultdict(int)
for monkey in monkeys:
    seen = set()
    changes_per_monkey = {}
    for i in range(4, len(monkey)):
        da = (monkey[i - 0] % 10) - (monkey[i - 1] % 10)
        db = (monkey[i - 1] % 10) - (monkey[i - 2] % 10)
        dc = (monkey[i - 2] % 10) - (monkey[i - 3] % 10)
        dd = (monkey[i - 3] % 10) - (monkey[i - 4] % 10)

        if (da, db, dc, dd) in seen:
            continue

        seen.add((da, db, dc, dd))
        price_changes[(da, db, dc, dd)] += monkey[i] % 10

print(sum(n[-1] for n in monkeys))
print(max(price_changes.values()))

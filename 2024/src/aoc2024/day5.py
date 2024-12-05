#!/usr/bin/env python

from collections import defaultdict
import sys

inp = sys.stdin.read().strip()
rules_str, updates = inp.split("\n\n")

rules = defaultdict(list)
for rule in rules_str.split():
    a, b = rule.split("|")
    rules[int(b)].append(int(a))

updates = [[int(n) for n in l.split(",")] for l in updates.split()]

p1 = 0
p2 = 0
for update in updates:
    update_set = set(update)

    new_update = {}
    to_add = set(update_set)
    while to_add:
        for n in to_add:
            if all(prev in new_update for prev in rules[n] if prev in update_set):
                new_update[n] = None
                to_add.remove(n)
                break

    new_update = list(new_update.keys())
    if new_update == update:
        p1 += update[len(update) // 2]
    else:
        p2 += new_update[len(new_update) // 2]


print(p1)
print(p2)

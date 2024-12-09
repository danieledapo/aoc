#!/usr/bin/env python3

import sys
import copy


def part1(blocks):
    blocks = copy.deepcopy(blocks)

    left = 0
    right = len(blocks) - 1

    while left < right:
        free_size, sid = blocks[left]
        if sid >= 0:
            left += 1
            continue

        block_size, id = blocks[right]
        if id < 0:
            right -= 1
            continue

        if free_size >= block_size:
            blocks[left] = (block_size, id)
            blocks[right] = (0, -1)
            if free_size - block_size > 0:
                blocks.insert(left + 1, (free_size - block_size, -1))
        else:
            blocks[left] = (free_size, id)
            blocks[right] = (block_size - free_size, id)

    return blocks


def part2(blocks):
    blocks = copy.deepcopy(blocks)

    free = 0
    while free < len(blocks):
        free_size, fid = blocks[free]
        if fid >= 0:
            free += 1
            continue

        try:
            _, bi = max(
                (blocks[i][1], i)
                for i in range(free + 1, len(blocks))
                if blocks[i][1] >= 0 and blocks[i][0] <= free_size
            )
        except ValueError:
            free += 1
            continue

        block_size = blocks[bi][0]
        blocks[free] = blocks[bi]
        blocks[bi] = (block_size, -1)
        if free_size - block_size > 0:
            blocks.insert(free + 1, (free_size - block_size, -1))

    return blocks


blocks = []

for i, n in enumerate(sys.stdin.read().strip()):
    blocks.append((int(n), -1 if i % 2 != 0 else i // 2))

for bb in (part1(blocks), part2(blocks)):
    res = 0
    pos = 0
    for n, id in bb:
        for _ in range(n):
            res += pos * max(0, id)
            pos += 1

    print(res)

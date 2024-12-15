#!/usr/bin/env python3

import sys


def robot_move(grid, moves):
    DIR_MAP = {"<": (0, -1), ">": (0, 1), "v": (1, 0), "^": (-1, 0)}
    height, width = len(grid), len(grid[0])

    pos = next((c, r) for r in range(height) for c in range(width) if grid[r][c] == "@")

    for move in moves:
        dr, dc = DIR_MAP[move]

        def move_if_able(changes, grid, x, y):
            curc = changes.get((x, y), grid[y][x])

            if curc == "#":
                return False

            if curc != ".":
                if not move_if_able(changes, grid, x + dc, y + dr):
                    return False

                if (
                    dr != 0
                    and curc == "["
                    and not move_if_able(changes, grid, x + 1 + dc, y + dr)
                ):
                    return False

                if (
                    dr != 0
                    and curc == "]"
                    and not move_if_able(changes, grid, x - 1 + dc, y + dr)
                ):
                    return False

            changes[(x, y)] = grid[y - dr][x - dc]
            changes[(x - dc, y - dr)] = "."
            return True

        changes = {}
        xx, yy = pos[0] + dc, pos[1] + dr
        if move_if_able(changes, grid, xx, yy):
            pos = xx, yy
            for (x, y), v in changes.items():
                grid[y][x] = v

    return sum(
        (r * 100 + c for r in range(height) for c in range(width) if grid[r][c] in "O[")
    )


inp = sys.stdin.read()

grid, moves = inp.strip().split("\n\n")
grid = [list(r) for r in grid.splitlines()]
moves = moves.replace("\n", "")

repl = {".": "..", "#": "##", "@": "@.", "O": "[]"}
grid2 = [list("".join(repl[c] for c in l)) for l in grid]

print(robot_move(grid, moves))
print(robot_move(grid2, moves))

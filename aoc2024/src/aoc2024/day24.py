#!/usr/bin/env python3

import sys
import tempfile
import subprocess


def run(values, gates):
    OPS = {"AND": int.__and__, "OR": int.__or__, "XOR": int.__xor__}

    gates = gates[:]
    while gates:
        for i, (g0, op, g1, dst) in enumerate(gates):
            if g0 in values and g1 in values:
                values[dst] = OPS[op](values[g0], values[g1])
                gates.pop(i)
                break

    return sum(values[k] << int(k[1:]) for k, v in values.items() if k.startswith("z"))


inp = sys.stdin.read().strip()

initial_str, gates_str = inp.split("\n\n")
values = {}
for l in initial_str.splitlines():
    wire, value = l.split(": ")
    values[wire] = int(value)

gates = []
for l in gates_str.splitlines():
    g0, op, g1, _, dst = l.split(" ")
    gates.append((g0, op, g1, dst))

print(run(values, gates))


with tempfile.NamedTemporaryFile("w") as fp:
    fp.write('graph "" {\n')
    for g0, op, g1, dst in gates:
        fp.write(f"""
    {g0} -- {g0}{op}{g1}
    {g1} -- {g0}{op}{g1}
    {g0}{op}{g1} -- {dst}
""")
    fp.write(r"}")
    fp.flush()

    subprocess.check_call(["dot", "-T", "svg", "-o", "day24.svg", fp.name])
print("inspect day24.svg manually :/")

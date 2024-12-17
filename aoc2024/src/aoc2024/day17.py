#!/usr/bin/env python3

import dataclasses
import sys
import re


@dataclasses.dataclass
class Machine:
    program: list[int]
    a: int = 0
    b: int = 0
    c: int = 0
    ip: int = 0
    out: list[int] = dataclasses.field(default_factory=list)

    def read_literal(self):
        n = self.program[self.ip]
        self.ip += 1
        return n

    def read_combo(self):
        n = self.read_literal()
        return [n, n, n, n, self.a, self.b, self.c][n]

    def read_op(self):
        op = self.program[self.ip]
        self.ip += 1

        if op == 0:
            self.a = int(self.a / 2 ** self.read_combo())
        elif op == 1:
            self.b ^= self.read_literal()
        elif op == 2:
            self.b = self.read_combo() % 8
        elif op == 3:
            addr = self.read_literal()
            if self.a != 0:
                self.ip = addr
        elif op == 4:
            self.read_literal()
            self.b ^= self.c
        elif op == 5:
            self.out.append(self.read_combo() % 8)
        elif op == 6:
            self.b = int(self.a / 2 ** self.read_combo())
        elif op == 7:
            self.c = int(self.a / 2 ** self.read_combo())
        else:
            assert False, op
        return op

    def run(self):
        while self.ip < len(self.program):
            self.read_op()
        return ",".join(map(str, self.out))


a, b, c, *program = map(int, re.findall(r"\d+", sys.stdin.read().strip()))


m = Machine(program=program, a=a, b=b, c=c)
print(m.run())


def reverse_run(a=0, i=0):
    m = Machine(program=program, a=a)
    m.run()
    if m.out == program:
        yield a

    if i <= 0 or m.out == program[-i:]:
        for j in range(8):
            yield from reverse_run(a * 8 + j, i + 1)


print(next(reverse_run()))

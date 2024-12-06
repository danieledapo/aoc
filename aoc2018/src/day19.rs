use std::str::FromStr;

type Registers = [usize; 6];

#[derive(Debug, PartialEq)]
struct Cpu {
    registers: Registers,
    ip: usize,
    ip_reg_no: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Instruction {
    Ip(usize),
    Addi(usize, usize, usize),
    Addr(usize, usize, usize),
    Muli(usize, usize, usize),
    Mulr(usize, usize, usize),
    Bani(usize, usize, usize),
    Banr(usize, usize, usize),
    Bori(usize, usize, usize),
    Borr(usize, usize, usize),
    Seti(usize, usize),
    Setr(usize, usize),
    Gtri(usize, usize, usize),
    Gtir(usize, usize, usize),
    Gtrr(usize, usize, usize),
    Eqri(usize, usize, usize),
    Eqir(usize, usize, usize),
    Eqrr(usize, usize, usize),
}

pub fn part1(input: &str) -> usize {
    let instructions: Vec<Instruction> = input.lines().map(|l| l.parse().unwrap()).collect();
    let (ip, instructions) = instructions.split_at(1);

    let mut cpu = Cpu {
        registers: [0; 6],
        ip_reg_no: match ip {
            [Instruction::Ip(ip)] => *ip,
            _ => panic!(),
        },
        ip: 0,
    };

    while let Some(instr) = instructions.get(cpu.ip) {
        cpu.exec(*instr);
    }

    cpu.registers[0]
}

///
/// registers:
/// - r0: 0 if part1 and 1 if part2
/// - r1: upper bound of for
/// - r2: temporary
/// - r3: ip
/// - r4: counter1
/// - r5: counter2
///
/// Disassembly of my input with a bit of explaination
/// ```asm
///     #ip 3
///
///     0: addi 3 16 3 ; goto 17
///
///     1: seti 1 8 5  ; r5 = 1
///     2: seti 1 0 4  ; r4 = 1
///     3: mulr 5 4 2  ; r2 = r5 * r4
///     4: eqrr 2 1 2  ; r2 = r2 == r1
///     5: addr 2 3 3  ; if r2 then goto 7 else goto 6
///     6: addi 3 1 3  ; goto 8
///     7: addr 5 0 0  ; r0 += r5
///     8: addi 4 1 4  ; r4 += 1
///     9: gtrr 4 1 2  ; r2 = r4 > r1
///    10: addr 3 2 3  ; if r2 then goto 12 else goto 11
///    11: seti 2 3 3  ; goto 2
///
///    12: addi 5 1 5  ; r5 += 1
///    13: gtrr 5 1 2  ; r2 = r5 > r1
///    14: addr 2 3 3  ; if r2 then goto 16 else goto 15
///    15: seti 1 4 3  ; goto 1
///    16: mulr 3 3 3  ; halt
///
///    17: addi 1 2 1  ; r1 += 2
///    18: mulr 1 1 1  ; r1 *= r1
///    19: mulr 3 1 1  ; r1 *= 19
///    20: muli 1 11 1 ; r1 *= 11
///    21: addi 2 4 2  ; r2 += 4
///    22: mulr 2 3 2  ; r2 *= 22
///    23: addi 2 19 2 ; r2 += 19
///    24: addr 1 2 1  ; r1 += r2
///    25: addr 3 0 3  ; if r0 then goto 27 else 26
///    26: seti 0 7 3  ; goto 1
///    27: setr 3 2 2  ; r2 = 27
///    28: mulr 2 3 2  ; r2 *= 28
///    29: addr 3 2 2  ; r2 += 29
///    30: mulr 3 2 2  ; r2 *= 30
///    31: muli 2 14 2 ; r2 *= 14
///    32: mulr 2 3 2  ; r2 *= 32
///    33: addr 1 2 1  ; r1 += r2
///    34: seti 0 1 0  ; r0 = 0
///    35: seti 0 5 3  ; goto 1
/// ```

pub fn part2(input: &str) -> usize {
    let instructions: Vec<Instruction> = input.lines().map(|l| l.parse().unwrap()).collect();
    let (ip, instructions) = instructions.split_at(1);

    let mut cpu = Cpu {
        registers: [1, 0, 0, 0, 0, 0],
        ip_reg_no: match ip {
            [Instruction::Ip(ip)] => *ip,
            _ => panic!(),
        },
        ip: 0,
    };

    while let Some(instr) = instructions.get(cpu.ip) {
        cpu.exec(*instr);

        // hardcoded start of body loop
        if cpu.ip == 1 {
            break;
        }
    }

    for r5 in 1..=cpu.registers[1] {
        cpu.registers[5] = r5;

        for r4 in 1..=cpu.registers[1] {
            cpu.registers[4] = r4;

            let r2 = cpu.registers[4] * cpu.registers[5];

            // optimization
            if r2 > cpu.registers[1] {
                break;
            }

            if r2 == cpu.registers[1] {
                cpu.registers[0] += cpu.registers[5];

                // optimization
                break;
            }
        }
    }

    cpu.registers[0]
}

impl Cpu {
    fn exec(&mut self, instr: Instruction) {
        if let Instruction::Ip(ip) = instr {
            self.ip_reg_no = ip;
            return;
        }

        self.registers[self.ip_reg_no] = self.ip;

        match instr {
            Instruction::Ip(_) => unreachable!(),
            Instruction::Addi(l, m, r) => self.addi(l, m, r),
            Instruction::Addr(l, m, r) => self.addr(l, m, r),
            Instruction::Muli(l, m, r) => self.muli(l, m, r),
            Instruction::Mulr(l, m, r) => self.mulr(l, m, r),
            Instruction::Bani(l, m, r) => self.bani(l, m, r),
            Instruction::Banr(l, m, r) => self.banr(l, m, r),
            Instruction::Bori(l, m, r) => self.bori(l, m, r),
            Instruction::Borr(l, m, r) => self.borr(l, m, r),
            Instruction::Seti(l, m) => self.seti(l, m),
            Instruction::Setr(l, m) => self.setr(l, m),
            Instruction::Gtri(l, m, r) => self.gtri(l, m, r),
            Instruction::Gtir(l, m, r) => self.gtir(l, m, r),
            Instruction::Gtrr(l, m, r) => self.gtrr(l, m, r),
            Instruction::Eqri(l, m, r) => self.eqri(l, m, r),
            Instruction::Eqir(l, m, r) => self.eqir(l, m, r),
            Instruction::Eqrr(l, m, r) => self.eqrr(l, m, r),
        };

        self.ip = self.registers[self.ip_reg_no];
        self.ip += 1;
    }

    fn addr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] + self.registers[reg_b];
    }

    fn addi(&mut self, reg_a: usize, value: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] + value;
    }

    fn mulr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] * self.registers[reg_b];
    }

    fn muli(&mut self, reg_a: usize, value: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] * value;
    }

    fn banr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] & self.registers[reg_b];
    }

    fn bani(&mut self, reg_a: usize, value: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] & value;
    }

    fn borr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] | self.registers[reg_b];
    }

    fn bori(&mut self, reg_a: usize, value: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a] | value;
    }

    fn setr(&mut self, reg_a: usize, reg_dest: usize) {
        self.registers[reg_dest] = self.registers[reg_a];
    }

    fn seti(&mut self, value: usize, reg_dest: usize) {
        self.registers[reg_dest] = value;
    }

    fn gtir(&mut self, value_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if value_a > self.registers[reg_b] {
            1
        } else {
            0
        };
    }

    fn gtri(&mut self, reg_a: usize, value_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if self.registers[reg_a] > value_b {
            1
        } else {
            0
        };
    }

    fn gtrr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if self.registers[reg_a] > self.registers[reg_b] {
            1
        } else {
            0
        };
    }

    fn eqir(&mut self, value_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if value_a == self.registers[reg_b] {
            1
        } else {
            0
        };
    }

    fn eqri(&mut self, reg_a: usize, value_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if self.registers[reg_a] == value_b {
            1
        } else {
            0
        };
    }

    fn eqrr(&mut self, reg_a: usize, reg_b: usize, reg_dest: usize) {
        self.registers[reg_dest] = if self.registers[reg_a] == self.registers[reg_b] {
            1
        } else {
            0
        };
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.split_whitespace();

        let opcode = parts.next().ok_or(())?;

        let mut next_int = || parts.next().ok_or(())?.parse().map_err(|_| ());

        if opcode == "#ip" {
            return Ok(Instruction::Ip(next_int()?));
        }

        let a1 = next_int()?;
        let a2 = next_int()?;
        let dst = next_int()?;

        let ins = match opcode {
            "addi" => Instruction::Addi(a1, a2, dst),
            "addr" => Instruction::Addr(a1, a2, dst),
            "muli" => Instruction::Muli(a1, a2, dst),
            "mulr" => Instruction::Mulr(a1, a2, dst),
            "bani" => Instruction::Bani(a1, a2, dst),
            "banr" => Instruction::Banr(a1, a2, dst),
            "bori" => Instruction::Bori(a1, a2, dst),
            "borr" => Instruction::Borr(a1, a2, dst),
            "seti" => Instruction::Seti(a1, dst),
            "setr" => Instruction::Setr(a1, dst),
            "gtri" => Instruction::Gtri(a1, a2, dst),
            "gtir" => Instruction::Gtir(a1, a2, dst),
            "gtrr" => Instruction::Gtrr(a1, a2, dst),
            "eqri" => Instruction::Eqri(a1, a2, dst),
            "eqir" => Instruction::Eqir(a1, a2, dst),
            "eqrr" => Instruction::Eqrr(a1, a2, dst),
            _ => unimplemented!(),
        };

        Ok(ins)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(1008, part1(include_str!("../input/day19.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(11_534_976, part2(include_str!("../input/day19.txt")));
    }
}

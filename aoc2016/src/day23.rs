type Reg = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arg {
    Reg(Reg),
    Imm(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Cpy(Arg, Arg),
    Incr(Arg),
    Decr(Arg),
    Jnz(Arg, Arg),
    Tgl(Arg),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Machine {
    registers: [i64; 4],
    instructions: Vec<Op>,
    ip: usize,
}

pub fn part1(input: &str) -> i64 {
    let mut machine = Machine::new(input);
    machine.registers[0] = 7;
    machine.run();
    machine.registers[0]
}

pub fn part2(input: &str) -> i64 {
    // fast enough in release mode
    let mut machine = Machine::new(input);
    machine.registers[0] = 12;
    machine.run();
    machine.registers[0]
}

impl Machine {
    pub fn run(&mut self) {
        while let Some(op) = self.instructions.get(self.ip) {
            self.ip = match *op {
                Op::Cpy(a, Arg::Reg(r)) => {
                    self.registers[r] = self.read(a);
                    self.ip + 1
                }
                Op::Incr(Arg::Reg(r)) => {
                    self.registers[r] += 1;
                    self.ip + 1
                }
                Op::Decr(Arg::Reg(r)) => {
                    self.registers[r] -= 1;
                    self.ip + 1
                }
                Op::Jnz(a, off) => {
                    if self.read(a) != 0 {
                        let off = self.read(off);
                        if off < 0 {
                            self.ip - off.abs() as usize
                        } else {
                            self.ip + off as usize
                        }
                    } else {
                        self.ip + 1
                    }
                }
                Op::Tgl(a) => {
                    let target = self.ip as i64 + self.read(a);
                    if target >= 0 && target < self.instructions.len() as i64 {
                        self.instructions[target as usize] =
                            match self.instructions[target as usize] {
                                Op::Incr(a) => Op::Decr(a),
                                Op::Decr(r) | Op::Tgl(r) => Op::Incr(r),

                                Op::Jnz(r, off) => Op::Cpy(r, off),
                                Op::Cpy(a, b) => Op::Jnz(a, b),
                            };
                    }

                    self.ip + 1
                }
                _ => self.ip + 1,
            };
            // println!("{:?}", self.registers);
        }
    }

    fn read(&self, a: Arg) -> i64 {
        match a {
            Arg::Imm(imm) => imm,
            Arg::Reg(r) => self.registers[r],
        }
    }

    pub fn new(input: &str) -> Self {
        let instructions = input
            .lines()
            .map(|l| {
                let mut parts = l.split_ascii_whitespace();

                let parse_arg = |w: &str| {
                    if let Ok(imm) = w.parse::<i64>() {
                        return Arg::Imm(imm);
                    }

                    assert_eq!(w.len(), 1);
                    Arg::Reg(usize::from(w.as_bytes()[0] - b'a'))
                };

                match parts.next().unwrap() {
                    "cpy" => {
                        let v = parse_arg(parts.next().unwrap());
                        let reg = parse_arg(parts.next().unwrap());
                        Op::Cpy(v, reg)
                    }
                    "inc" => Op::Incr(parse_arg(parts.next().unwrap())),
                    "dec" => Op::Decr(parse_arg(parts.next().unwrap())),
                    "jnz" => {
                        let v = parse_arg(parts.next().unwrap());
                        let off = parse_arg(parts.next().unwrap());
                        Op::Jnz(v, off)
                    }
                    "tgl" => {
                        let v = parse_arg(parts.next().unwrap());
                        Op::Tgl(v)
                    }
                    a => unreachable!("{}", a),
                }
            })
            .collect();

        Self {
            instructions,
            registers: [0; 4],
            ip: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day23.txt")), 12315);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day23.txt")), 479008875);
    }
}

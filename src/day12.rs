type Reg = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arg {
    Reg(Reg),
    Imm(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Cpy(Arg, Reg),
    Incr(Reg),
    Decr(Reg),
    Jnz(Arg, i16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Machine {
    registers: [i64; 4],
    instructions: Vec<Op>,
    ip: usize,
}

pub fn part1(input: &str) -> i64 {
    let mut machine = Machine::new(input);
    machine.run();
    machine.registers[0]
}

pub fn part2(input: &str) -> i64 {
    let mut machine = Machine::new(input);
    machine.registers[2] = 1;
    machine.run();
    machine.registers[0]
}

impl Machine {
    pub fn run(&mut self) {
        while let Some(op) = self.instructions.get(self.ip) {
            self.ip = match *op {
                Op::Cpy(a, r) => {
                    self.registers[r] = self.read(a);
                    self.ip + 1
                }
                Op::Incr(r) => {
                    self.registers[r] += 1;
                    self.ip + 1
                }
                Op::Decr(r) => {
                    self.registers[r] -= 1;
                    self.ip + 1
                }
                Op::Jnz(a, off) => {
                    if self.read(a) != 0 {
                        if off < 0 {
                            self.ip - usize::from(off.abs() as u16)
                        } else {
                            self.ip + usize::from(off as u16)
                        }
                    } else {
                        self.ip + 1
                    }
                }
            };
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
                let parse_reg = |w| match parse_arg(w) {
                    Arg::Reg(r) => r,
                    _ => unreachable!(),
                };

                match parts.next().unwrap() {
                    "cpy" => {
                        let v = parse_arg(parts.next().unwrap());
                        let reg = parse_reg(parts.next().unwrap());
                        Op::Cpy(v, reg)
                    }
                    "inc" => Op::Incr(parse_reg(parts.next().unwrap())),
                    "dec" => Op::Decr(parse_reg(parts.next().unwrap())),
                    "jnz" => {
                        let v = parse_arg(parts.next().unwrap());
                        let off = parts.next().unwrap().parse().unwrap();
                        Op::Jnz(v, off)
                    }
                    _ => unreachable!(),
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
        assert_eq!(part1(include_str!("../input/day12.txt")), 318003);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day12.txt")), 9227657);
    }
}

use std::{collections::HashSet, str::FromStr};

#[derive(Clone)]
pub struct Cpu {
    pub program: Vec<Op>,
    pub ip: i16,
    pub acc: i32,
}

#[derive(Clone)]
pub enum Op {
    Nop(i16),
    Jmp(i16),
    Acc(i32),
}

pub fn part1(input: &str) -> i32 {
    let mut cpu = Cpu::new(parse(input).collect());

    let mut seen = HashSet::new();
    while seen.insert(cpu.ip) {
        cpu.step();
    }

    cpu.acc
}

pub fn part2(input: &str) -> i32 {
    let program: Vec<_> = parse(input).collect();

    let patchable = program.iter().enumerate().filter(|(_, op)| match op {
        Op::Jmp(_) | Op::Nop(_) => true,
        Op::Acc(_) => false,
    });

    for (i, op) in patchable {
        let mut patched = program.clone();
        patched[i] = match op {
            Op::Jmp(ro) => Op::Nop(*ro),
            Op::Nop(ro) => Op::Jmp(*ro),
            Op::Acc(_) => unreachable!(),
        };

        let mut cpu = Cpu::new(patched);

        let mut seen = HashSet::new();
        while seen.insert(cpu.ip) {
            if cpu.step() {
                return cpu.acc;
            }
        }
    }

    unreachable!()
}

fn parse(input: &str) -> impl Iterator<Item = Op> + '_ {
    input.lines().map(|l| l.parse().unwrap())
}

impl Cpu {
    pub fn new(program: Vec<Op>) -> Self {
        Self {
            program,
            ip: 0,
            acc: 0,
        }
    }

    pub fn step(&mut self) -> bool {
        assert!(self.ip >= 0);

        let op = match self.program.get(usize::from(self.ip as u16)) {
            None => return true,
            Some(op) => op,
        };

        match op {
            Op::Nop(_) => {
                self.ip += 1;
            }
            Op::Jmp(ro) => {
                self.ip += ro;
            }
            Op::Acc(a) => {
                self.acc += a;
                self.ip += 1;
            }
        }

        false
    }
}

impl FromStr for Op {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut f = s.split_whitespace();
        match f.next() {
            Some("nop") => {
                let n = f.next().ok_or(())?.parse().map_err(|_| ())?;
                Ok(Op::Nop(n))
            }
            Some("jmp") => {
                let n = f.next().ok_or(())?.parse().map_err(|_| ())?;
                assert!(f.next().is_none());
                Ok(Op::Jmp(n))
            }
            Some("acc") => {
                let n = f.next().ok_or(())?.parse().map_err(|_| ())?;
                assert!(f.next().is_none());
                Ok(Op::Acc(n))
            }
            _ => {
                dbg!(s);
                Err(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(1521, part1(include_str!("../input/day8.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1016, part2(include_str!("../input/day8.txt")));
    }
}

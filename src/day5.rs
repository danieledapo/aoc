use std::convert::TryFrom;

pub fn part1(inp: &str) -> i64 {
    *Machine::new(inp).run(&mut vec![1]).last().unwrap()
}

pub fn part2(inp: &str) -> i64 {
    *Machine::new(inp).run(&mut vec![5]).last().unwrap()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RunMode {
    TilHalt,
    YieldOutput,
    Yield,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Machine {
    pub ip: usize,
    pub prog: Vec<i64>,
    pub run_mode: RunMode,
    pub relative_offset: i64,
}

impl Machine {
    pub fn new(program: &str) -> Self {
        Self::with_run_mode(program, RunMode::TilHalt)
    }

    pub fn with_run_mode(program: &str, run_mode: RunMode) -> Self {
        Machine {
            ip: 0,
            relative_offset: 0,
            prog: parse(program),
            run_mode,
        }
    }

    pub fn run(&mut self, inputs: &mut Vec<i64>) -> Vec<i64> {
        let mut outputs = vec![];

        while self.prog[self.ip] != 99 {
            let op = self.prog[self.ip] % 100;
            let mode1 = self.prog[self.ip] / 100 % 10;
            let mode2 = self.prog[self.ip] / 1_000 % 10;
            let mode3 = self.prog[self.ip] / 10_000 % 10;

            if op == 1 || op == 2 {
                let im1 = self.read_value(self.prog[self.ip + 1], mode1);
                let im2 = self.read_value(self.prog[self.ip + 2], mode2);

                self.write_value(
                    self.prog[self.ip + 3],
                    match op {
                        1 => im1 + im2,
                        2 => im1 * im2,
                        _ => unreachable!(),
                    },
                    mode3,
                );

                self.ip += 4;
                continue;
            }

            if op == 3 {
                if inputs.is_empty() && self.run_mode == RunMode::Yield {
                    break;
                }

                self.write_value(self.prog[self.ip + 1], inputs.remove(0), mode1);
                self.ip += 2;
                continue;
            }

            if op == 4 {
                outputs.push(self.read_value(self.prog[self.ip + 1], mode1));
                self.ip += 2;

                match self.run_mode {
                    RunMode::YieldOutput | RunMode::Yield => break,
                    RunMode::TilHalt => continue,
                }
            }

            if op == 5 {
                if self.read_value(self.prog[self.ip + 1], mode1) != 0 {
                    self.ip =
                        usize::try_from(self.read_value(self.prog[self.ip + 2], mode2)).unwrap();
                } else {
                    self.ip += 3;
                }
                continue;
            }

            if op == 6 {
                if self.read_value(self.prog[self.ip + 1], mode1) == 0 {
                    self.ip =
                        usize::try_from(self.read_value(self.prog[self.ip + 2], mode2)).unwrap();
                } else {
                    self.ip += 3;
                }
                continue;
            }

            if op == 7 {
                let im1 = self.read_value(self.prog[self.ip + 1], mode1);
                let im2 = self.read_value(self.prog[self.ip + 2], mode2);
                self.write_value(self.prog[self.ip + 3], if im1 < im2 { 1 } else { 0 }, mode3);
                self.ip += 4;
                continue;
            }

            if op == 8 {
                let im1 = self.read_value(self.prog[self.ip + 1], mode1);
                let im2 = self.read_value(self.prog[self.ip + 2], mode2);
                self.write_value(
                    self.prog[self.ip + 3],
                    if im1 == im2 { 1 } else { 0 },
                    mode3,
                );
                self.ip += 4;
                continue;
            }

            if op == 9 {
                let im1 = self.read_value(self.prog[self.ip + 1], mode1);
                self.relative_offset += im1;
                self.ip += 2;
                continue;
            }

            unreachable!("opcode: {}", op);
        }

        outputs
    }

    fn read_value(&self, arg: i64, mode: i64) -> i64 {
        match mode {
            0 => {
                let d = usize::try_from(arg).unwrap();
                *self.prog.get(d).unwrap_or(&0)
            }
            1 => arg,
            2 => {
                let d = usize::try_from(arg + self.relative_offset).unwrap();
                *self.prog.get(d).unwrap_or(&0)
            }
            _ => unreachable!("mode: {}", mode),
        }
    }

    fn write_value(&mut self, dest: i64, value: i64, mode: i64) {
        let dest =
            usize::try_from(dest + if mode == 2 { self.relative_offset } else { 0 }).unwrap();
        if dest >= self.prog.len() {
            self.prog
                .extend(std::iter::repeat(0).take(dest - self.prog.len() + 1));
        }

        self.prog[dest] = value;
    }
}

fn parse(inp: &str) -> Vec<i64> {
    inp.trim().split(',').map(|n| n.parse().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day5.txt")), 13818007);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day5.txt")), 3176266);
    }
}

use std::convert::TryFrom;

pub fn part1(inp: &str) -> i32 {
    *Machine::new(inp).run(&mut vec![1]).last().unwrap()
}

pub fn part2(inp: &str) -> i32 {
    *Machine::new(inp).run(&mut vec![5]).last().unwrap()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RunMode {
    TilHalt,
    YieldOutput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Machine {
    ip: usize,
    prog: Vec<i32>,
    run_mode: RunMode,
}

impl Machine {
    pub fn new(program: &str) -> Self {
        Self::with_run_mode(program, RunMode::TilHalt)
    }

    pub fn with_run_mode(program: &str, run_mode: RunMode) -> Self {
        Machine {
            ip: 0,
            prog: parse(program),
            run_mode,
        }
    }

    pub fn run(&mut self, inputs: &mut Vec<i32>) -> Vec<i32> {
        let get_value = |prog: &[i32], arg: i32, mode: i32| match mode {
            0 => prog[usize::try_from(arg).unwrap()],
            1 => arg,
            _ => unreachable!(),
        };

        let mut outputs = vec![];

        while self.prog[self.ip] != 99 {
            let op = self.prog[self.ip] % 100;
            let mode1 = self.prog[self.ip] / 100 % 10;
            let mode2 = self.prog[self.ip] / 1_000 % 10;

            if op == 1 || op == 2 {
                let im1 = get_value(&self.prog, self.prog[self.ip + 1], mode1);
                let im2 = get_value(&self.prog, self.prog[self.ip + 2], mode2);
                let dst = usize::try_from(self.prog[self.ip + 3]).unwrap();

                self.prog[dst] = match op {
                    1 => im1 + im2,
                    2 => im1 * im2,
                    _ => unreachable!(),
                };

                self.ip += 4;
                continue;
            }

            if op == 3 {
                let dst = usize::try_from(self.prog[self.ip + 1]).unwrap();
                self.prog[dst] = inputs.remove(0);
                self.ip += 2;
                continue;
            }

            if op == 4 {
                outputs.push(get_value(&self.prog, self.prog[self.ip + 1], mode1));
                self.ip += 2;

                if self.run_mode == RunMode::YieldOutput {
                    break;
                } else {
                    continue;
                }
            }

            if op == 5 {
                if get_value(&self.prog, self.prog[self.ip + 1], mode1) != 0 {
                    self.ip = usize::try_from(get_value(&self.prog, self.prog[self.ip + 2], mode2))
                        .unwrap();
                } else {
                    self.ip += 3;
                }
                continue;
            }

            if op == 6 {
                if get_value(&self.prog, self.prog[self.ip + 1], mode1) == 0 {
                    self.ip = usize::try_from(get_value(&self.prog, self.prog[self.ip + 2], mode2))
                        .unwrap();
                } else {
                    self.ip += 3;
                }
                continue;
            }

            if op == 7 {
                let im1 = get_value(&self.prog, self.prog[self.ip + 1], mode1);
                let im2 = get_value(&self.prog, self.prog[self.ip + 2], mode2);
                let dst = usize::try_from(self.prog[self.ip + 3]).unwrap();
                self.prog[dst] = if im1 < im2 { 1 } else { 0 };
                self.ip += 4;
                continue;
            }

            if op == 8 {
                let im1 = get_value(&self.prog, self.prog[self.ip + 1], mode1);
                let im2 = get_value(&self.prog, self.prog[self.ip + 2], mode2);
                let dst = usize::try_from(self.prog[self.ip + 3]).unwrap();
                self.prog[dst] = if im1 == im2 { 1 } else { 0 };
                self.ip += 4;
                continue;
            }

            unreachable!();
        }

        outputs
    }
}

fn parse(inp: &str) -> Vec<i32> {
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

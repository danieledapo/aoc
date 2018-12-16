use std::collections::hash_map::Entry;
use std::collections::HashMap;

type Registers = [u16; 4];
type Instruction = [u16; 4];

#[derive(Debug, PartialEq)]
struct Cpu {
    registers: Registers,
}

impl Cpu {
    fn addr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            self.registers[usize::from(reg_a)] + self.registers[usize::from(reg_b)];
    }

    fn addi(&mut self, reg_a: u16, value: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = self.registers[usize::from(reg_a)] + value;
    }

    fn mulr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            self.registers[usize::from(reg_a)] * self.registers[usize::from(reg_b)];
    }

    fn muli(&mut self, reg_a: u16, value: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = self.registers[usize::from(reg_a)] * value;
    }

    fn banr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            self.registers[usize::from(reg_a)] & self.registers[usize::from(reg_b)];
    }

    fn bani(&mut self, reg_a: u16, value: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = self.registers[usize::from(reg_a)] & value;
    }

    fn borr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            self.registers[usize::from(reg_a)] | self.registers[usize::from(reg_b)];
    }

    fn bori(&mut self, reg_a: u16, value: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = self.registers[usize::from(reg_a)] | value;
    }

    fn setr(&mut self, reg_a: u16, _b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = self.registers[usize::from(reg_a)];
    }

    fn seti(&mut self, value: u16, _b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = value;
    }

    fn gtir(&mut self, value_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = if value_a > self.registers[usize::from(reg_b)] {
            1
        } else {
            0
        };
    }

    fn gtri(&mut self, reg_a: u16, value_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = if self.registers[usize::from(reg_a)] > value_b {
            1
        } else {
            0
        };
    }

    fn gtrr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            if self.registers[usize::from(reg_a)] > self.registers[usize::from(reg_b)] {
                1
            } else {
                0
            };
    }

    fn eqir(&mut self, value_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = if value_a == self.registers[usize::from(reg_b)] {
            1
        } else {
            0
        };
    }

    fn eqri(&mut self, reg_a: u16, value_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] = if self.registers[usize::from(reg_a)] == value_b {
            1
        } else {
            0
        };
    }

    fn eqrr(&mut self, reg_a: u16, reg_b: u16, reg_dest: u16) {
        self.registers[usize::from(reg_dest)] =
            if self.registers[usize::from(reg_a)] == self.registers[usize::from(reg_b)] {
                1
            } else {
                0
            };
    }
}

const INSTRUCTIONS_FNS: [fn(&mut Cpu, u16, u16, u16); 16] = [
    Cpu::addr,
    Cpu::addi,
    Cpu::mulr,
    Cpu::muli,
    Cpu::banr,
    Cpu::bani,
    Cpu::borr,
    Cpu::bori,
    Cpu::setr,
    Cpu::seti,
    Cpu::gtir,
    Cpu::gtri,
    Cpu::gtrr,
    Cpu::eqir,
    Cpu::eqri,
    Cpu::eqrr,
];

pub fn part1(input: &str) -> usize {
    let (samples, _) = split_input(input).unwrap();

    let hypo = build_hypotheses_per_instruction(samples.lines());
    hypo.into_iter().filter(|(_, ins)| ins.len() >= 3).count()
}

pub fn part2(input: &str) -> u16 {
    let (samples, program) = split_input(input).unwrap();

    let hypo = build_hypotheses_per_instruction(samples.lines());

    let mut deps = hypo.into_iter().fold(
        HashMap::new(),
        |mut deps, (instruction, plausible_instructions)| {
            let opcode = usize::from(instruction[0]);

            match deps.entry(opcode) {
                Entry::Vacant(v) => {
                    v.insert(plausible_instructions);
                }
                Entry::Occupied(mut o) => {
                    if plausible_instructions.len() < o.get().len() {
                        *o.get_mut() = plausible_instructions;
                    }
                }
            };

            deps
        },
    );

    let mut instructions = HashMap::new();

    while !deps.is_empty() {
        let leaves = deps
            .iter()
            .filter(|(_, ps)| ps.len() == 1)
            .map(|(opcode, ps)| (*opcode, ps[0]))
            .collect::<Vec<_>>();

        deps.retain(|_, ps| ps.len() > 1);

        for (_, fid) in &leaves {
            for possible in deps.values_mut() {
                if let Some(ix) = possible.iter().position(|p| p == fid) {
                    possible.swap_remove(ix);
                }
            }
        }

        instructions.extend(leaves);
    }

    let mut cpu = Cpu { registers: [0; 4] };

    for instruction in program.lines().map(|l| parse_instruction(l).unwrap()) {
        let fid = instructions[&usize::from(instruction[0])];

        let f = INSTRUCTIONS_FNS[fid];
        f(&mut cpu, instruction[1], instruction[2], instruction[3]);
    }

    cpu.registers[0]
}

fn build_hypotheses_per_instruction<'a>(
    samples: impl Iterator<Item = &'a str>,
) -> Vec<([u16; 4], Vec<usize>)> {
    let mut samples = samples.peekable();

    let mut res = vec![];

    while samples.peek().is_some() {
        let start_state = parse_registers("Before: ", samples.next().unwrap()).unwrap();
        let instruction = parse_instruction(samples.next().unwrap()).unwrap();
        let end_state = parse_registers("After:  ", samples.next().unwrap()).unwrap();
        // eat newline
        samples.next();

        let plausible_instructions = INSTRUCTIONS_FNS
            .iter()
            .enumerate()
            .filter_map(|(fid, f)| {
                let mut cpu = Cpu {
                    registers: start_state,
                };

                f(&mut cpu, instruction[1], instruction[2], instruction[3]);

                if cpu.registers == end_state {
                    Some(fid)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        res.push((instruction, plausible_instructions));
    }

    res
}

fn split_input(input: &str) -> Option<(&str, &str)> {
    let mut parts = input.split("\n\n\n\n");
    let samples = parts.next()?;
    let program = parts.next()?;

    Some((samples, program))
}

fn parse_registers(start: &str, s: &str) -> Option<Registers> {
    let mut nums = s
        .trim_left_matches(start)
        .trim_left_matches('[')
        .trim_right_matches(']')
        .split(", ");

    let ra = nums.next()?.parse::<u16>().ok()?;
    let rb = nums.next()?.parse::<u16>().ok()?;
    let rc = nums.next()?.parse::<u16>().ok()?;
    let rd = nums.next()?.parse::<u16>().ok()?;

    Some([ra, rb, rc, rd])
}

fn parse_instruction(line: &str) -> Option<Instruction> {
    let mut nums = line.split_whitespace();

    let opcode = nums.next()?.parse::<u16>().ok()?;
    let inputa = nums.next()?.parse::<u16>().ok()?;
    let inputb = nums.next()?.parse::<u16>().ok()?;
    let dest = nums.next()?.parse::<u16>().ok()?;

    Some([opcode, inputa, inputb, dest])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(642, part1(include_str!("../input/day16.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(481, part2(include_str!("../input/day16.txt")));
    }
}

use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Cpu {
    registers: [u32; 2],
    instructions: Vec<Instruction>,
    pc: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
    Half(usize),
    Triple(usize),
    Inc(usize),
    Jump(isize),
    JumpIfEven(usize, isize),
    JumpIfOne(usize, isize),
}

pub fn part1(input: &str) -> u32 {
    let mut cpu: Cpu = input.parse().unwrap();
    cpu.exec();
    cpu.registers[1]
}

pub fn part2(input: &str) -> u32 {
    let mut cpu: Cpu = input.parse().unwrap();
    cpu.registers[0] = 1;
    cpu.exec();
    cpu.registers[1]
}

impl Cpu {
    fn exec(&mut self) {
        while let Some(ins) = self.instructions.get(self.pc) {
            let off = match ins {
                Instruction::Half(r) => {
                    self.registers[*r] /= 2;
                    1
                }
                Instruction::Triple(r) => {
                    self.registers[*r] *= 3;
                    1
                }
                Instruction::Inc(r) => {
                    self.registers[*r] += 1;
                    1
                }
                Instruction::Jump(o) => *o,
                Instruction::JumpIfEven(r, o) => {
                    if self.registers[*r] % 2 == 0 {
                        *o
                    } else {
                        1
                    }
                }
                Instruction::JumpIfOne(r, o) => {
                    if self.registers[*r] == 1 {
                        *o
                    } else {
                        1
                    }
                }
            };

            if off < 0 {
                if off.abs() as usize > self.pc {
                    break;
                }

                self.pc -= off.abs() as usize;
            } else {
                self.pc += off as usize;
            }
        }
    }
}

impl FromStr for Cpu {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let instructions = input
            .trim()
            .lines()
            .map(|l| l.parse())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Cpu {
            registers: [0, 0],
            pc: 0,
            instructions,
        })
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (opcode, args) = input.split_at(4);
        let opcode = opcode.trim();
        let mut args = args.split(", ");

        let parse_reg = |a: &mut std::str::Split<&str>| match a.next().ok_or(())? {
            "a" => Ok(0),
            "b" => Ok(1),
            _ => Err(()),
        };
        let parse_num = |a: &mut std::str::Split<&str>| a.next().ok_or(())?.parse().map_err(|_| ());

        if opcode == "jmp" {
            return Ok(Instruction::Jump(parse_num(&mut args)?));
        }

        let reg = parse_reg(&mut args)?;

        match opcode {
            "hlf" => Ok(Instruction::Half(reg)),
            "tpl" => Ok(Instruction::Triple(reg)),
            "inc" => Ok(Instruction::Inc(reg)),
            "jie" => Ok(Instruction::JumpIfEven(reg, parse_num(&mut args)?)),
            "jio" => Ok(Instruction::JumpIfOne(reg, parse_num(&mut args)?)),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(170, part1(include_str!("../input/day23.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(247, part2(include_str!("../input/day23.txt")));
    }
}

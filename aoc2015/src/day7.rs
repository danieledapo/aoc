use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Cpu<'a> {
    wires: HashMap<&'a str, Instruction<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Instruction<'a> {
    dest: &'a str,
    opcode: OpCode<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum OpCode<'a> {
    Set(Operand<'a>),
    Not(Operand<'a>),
    And(Operand<'a>, Operand<'a>),
    Or(Operand<'a>, Operand<'a>),
    LShift(Operand<'a>, Operand<'a>),
    RShift(Operand<'a>, Operand<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Operand<'a> {
    Imm(u16),
    Reg(&'a str),
}

pub fn part1(input: &str) -> u16 {
    let cpu = Cpu::parse(input).unwrap();
    cpu.signal("a", &mut HashMap::new())
}

pub fn part2(input: &str) -> u16 {
    let mut cpu = Cpu::parse(input).unwrap();
    let signal_a = cpu.signal("a", &mut HashMap::new());

    cpu.wires.insert(
        "b",
        Instruction {
            dest: "b",
            opcode: OpCode::Set(Operand::Imm(signal_a)),
        },
    );

    cpu.signal("a", &mut HashMap::new())
}

impl<'a> Cpu<'a> {
    fn parse(input: &'a str) -> Option<Self> {
        let wires = input
            .lines()
            .map(|l| Instruction::parse(l).map(|i| (i.dest, i)))
            .collect::<Option<HashMap<_, _>>>()?;

        Some(Cpu { wires })
    }

    fn signal<'s: 'a>(self: &'s Self, wire: &'a str, cache: &mut HashMap<&'a str, u16>) -> u16 {
        if let Some(v) = cache.get(wire) {
            return *v;
        }

        let mut operand_value = |operand: &'s Operand| -> u16 {
            match operand {
                Operand::Imm(imm) => *imm,
                Operand::Reg(reg) => self.signal(reg, cache),
            }
        };

        let v = match self.wires.get(wire) {
            None => 0,
            Some(instr) => match &instr.opcode {
                OpCode::Set(op) => operand_value(op),
                OpCode::Not(op) => !operand_value(op),
                OpCode::And(lhs, rhs) => {
                    let l = operand_value(lhs);
                    let r = operand_value(rhs);
                    l & r
                }
                OpCode::Or(lhs, rhs) => {
                    let l = operand_value(lhs);
                    let r = operand_value(rhs);
                    l | r
                }
                OpCode::LShift(lhs, rhs) => {
                    let l = operand_value(lhs);
                    let s = operand_value(rhs);
                    l << s
                }
                OpCode::RShift(lhs, rhs) => {
                    let l = operand_value(lhs);
                    let s = operand_value(rhs);
                    l >> s
                }
            },
        };

        cache.insert(wire, v);

        v
    }
}

impl<'a> Instruction<'a> {
    fn parse(input: &'a str) -> Option<Self> {
        let mut parts = input.split(" -> ");
        let opcode = OpCode::parse(parts.next()?)?;
        let dest = parts.next()?;

        Some(Instruction { opcode, dest })
    }
}

impl<'a> OpCode<'a> {
    fn parse(input: &'a str) -> Option<Self> {
        let parts = input.split_whitespace().collect::<Vec<_>>();

        if parts.is_empty() {
            return None;
        }

        let parse_operand =
            |s: &'a str| s.parse().ok().map_or_else(|| Operand::Reg(s), Operand::Imm);

        if parts.len() == 1 {
            return Some(OpCode::Set(parse_operand(parts[0])));
        }

        if parts[0] == "NOT" {
            return Some(OpCode::Not(parse_operand(parts.get(1)?)));
        }

        let lhs = parse_operand(parts[0]);
        let rhs = parse_operand(parts.get(2)?);

        let opcode = match parts[1] {
            "AND" => OpCode::And(lhs, rhs),
            "OR" => OpCode::Or(lhs, rhs),
            "LSHIFT" => OpCode::LShift(lhs, rhs),
            "RSHIFT" => OpCode::RShift(lhs, rhs),
            _ => return None,
        };

        Some(opcode)
    }
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} -> {}", self.opcode, self.dest)
    }
}

impl Display for OpCode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpCode::Set(Operand::Imm(imm)) => write!(f, "{}", imm),
            OpCode::Set(Operand::Reg(reg)) => write!(f, "{}", reg),
            OpCode::And(lhs, rhs) => write!(f, "{} AND {}", lhs, rhs),
            OpCode::Or(lhs, rhs) => write!(f, "{} OR {}", lhs, rhs),
            OpCode::LShift(lhs, rhs) => write!(f, "{} LSHIFT {}", lhs, rhs),
            OpCode::RShift(lhs, rhs) => write!(f, "{} RSHIFT {}", lhs, rhs),
            OpCode::Not(lhs) => write!(f, "NOT {}", lhs),
        }
    }
}

impl Display for Operand<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Imm(imm) => write!(f, "{}", imm),
            Operand::Reg(reg) => write!(f, "{}", reg),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(3176, part1(include_str!("../input/day7.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(14710, part2(include_str!("../input/day7.txt")));
    }

}

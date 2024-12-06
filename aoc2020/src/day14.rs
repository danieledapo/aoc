use std::collections::HashMap;

#[derive(Clone)]
enum Op {
    SetMask(Vec<MaskBit>),
    SetMem { addr: u64, value: u64 },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MaskBit {
    On,
    Off,
    Floating,
}

pub fn part1(input: &str) -> u64 {
    let mut mem = HashMap::new();

    let mut mask_and = 0;
    let mut mask_or = 0;
    for op in parse(input) {
        match op {
            Op::SetMask(mask) => {
                mask_and = 0;
                mask_or = 0;
                for b in mask {
                    mask_or = (mask_or << 1) | u64::from(b == MaskBit::On);
                    mask_and = (mask_and << 1) | u64::from(b == MaskBit::Floating);
                }
            }
            Op::SetMem { addr, value } => {
                mem.insert(addr, (value & mask_and) | mask_or);
            }
        }
    }

    mem.values().sum()
}

pub fn part2(input: &str) -> u64 {
    let mut mem: HashMap<u64, u64> = HashMap::new();

    let mut mask = vec![];
    for op in parse(input) {
        match op {
            Op::SetMask(m) => {
                mask = m;
            }
            Op::SetMem { addr, value } => {
                let mut addresses = vec![0];

                for (bi, &b) in mask.iter().enumerate() {
                    match b {
                        MaskBit::On | MaskBit::Off => {
                            let addrb = (addr >> (mask.len() - 1 - bi)) & 1;
                            let newb = addrb | u64::from(b == MaskBit::On);

                            for a in &mut addresses {
                                *a = (*a << 1) | newb;
                            }
                        }
                        MaskBit::Floating => {
                            let n = addresses.len();
                            for i in 0..n {
                                addresses[i] <<= 1;
                                addresses.push(addresses[i] | 1);
                            }
                        }
                    }
                }

                for addr in addresses {
                    mem.insert(addr, value);
                }
            }
        }
    }

    mem.values().sum()
}

fn parse(input: &str) -> impl Iterator<Item = Op> + '_ {
    input.lines().map(|l| l.parse().unwrap())
}

impl std::str::FromStr for Op {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(mask) = s.strip_prefix("mask = ") {
            let mask: Result<Vec<_>, _> = mask
                .chars()
                .map(|c| match c {
                    '1' => Ok(MaskBit::On),
                    '0' => Ok(MaskBit::Off),
                    'X' => Ok(MaskBit::Floating),
                    _ => Err(()),
                })
                .collect();
            return Ok(Op::SetMask(mask?));
        }

        let mut parts = s.split(" = ");
        let addr = parts
            .next()
            .ok_or(())?
            .trim_start_matches("mem[")
            .trim_end_matches(']')
            .parse()
            .map_err(|_| ())?;
        let value = parts.next().ok_or(())?.parse().map_err(|_| ())?;
        Ok(Op::SetMem { addr, value })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(14839536808842, part1(include_str!("../input/day14.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(4215284199669, part2(include_str!("../input/day14.txt")));
    }
}

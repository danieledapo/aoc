use std::cmp::Reverse;

use crate::utils::lcm;

pub struct Monkey {
    pub items: Vec<u128>,
    pub op: fn(u128) -> u128,
    pub div_by: u128,
    pub throw_to: (usize, usize),
}

pub fn part1(monkeys: Vec<Monkey>) -> usize {
    go(20, 3, monkeys)
}

pub fn part2(monkeys: Vec<Monkey>) -> usize {
    go(10_000, 1, monkeys)
}

fn go(rounds: usize, div: u128, mut monkeys: Vec<Monkey>) -> usize {
    let mut inspected = vec![0; monkeys.len()];

    let mut mul = lcm(monkeys[0].div_by, monkeys[1].div_by);
    for m in &monkeys[2..] {
        mul = lcm(m.div_by, mul);
    }

    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            inspected[i] += monkeys[i].items.len();

            let items = std::mem::take(&mut monkeys[i].items);

            for item in items {
                let worry = (monkeys[i].op)(item) / div;
                let new_monkey = if worry % monkeys[i].div_by == 0 {
                    monkeys[i].throw_to.0
                } else {
                    monkeys[i].throw_to.1
                };

                monkeys[new_monkey].items.push(worry % mul);
            }
        }
    }

    inspected.sort_by_key(|n| Reverse(*n));
    inspected[0] * inspected[1]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_monkeys() -> Vec<Monkey> {
        vec![
            Monkey {
                items: vec![57, 58],
                op: |o| o * 19,
                div_by: 7,
                throw_to: (2, 3),
            },
            Monkey {
                items: vec![66, 52, 59, 79, 94, 73],
                op: |o| o + 1,
                div_by: 19,
                throw_to: (4, 6),
            },
            Monkey {
                items: vec![80],
                op: |o| o + 6,
                div_by: 5,
                throw_to: (7, 5),
            },
            Monkey {
                items: vec![82, 81, 68, 66, 71, 83, 75, 97],
                op: |o| o + 5,
                div_by: 11,
                throw_to: (5, 2),
            },
            Monkey {
                items: vec![55, 52, 67, 70, 69, 94, 90],
                op: |o| o * o,
                div_by: 17,
                throw_to: (0, 3),
            },
            Monkey {
                items: vec![69, 85, 89, 91],
                op: |o| o + 7,
                div_by: 13,
                throw_to: (1, 7),
            },
            Monkey {
                items: vec![75, 53, 73, 52, 75],
                op: |o| o * 7,
                div_by: 2,
                throw_to: (0, 4),
            },
            Monkey {
                items: vec![94, 60, 79],
                op: |o| o + 2,
                div_by: 3,
                throw_to: (1, 6),
            },
        ]
    }

    #[test]
    pub fn test_part1() {
        assert_eq!(50830, part1(get_monkeys()));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(14399640002, part2(get_monkeys()));
    }
}

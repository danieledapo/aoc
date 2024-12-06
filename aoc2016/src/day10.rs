use std::collections::HashMap;

type BotId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Bot {
    id: BotId,
    low_to: Dest,
    high_to: Dest,

    values: Vec<i64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dest {
    Bot(BotId),
    Output(usize),
}

pub fn part1(input: &str) -> BotId {
    let mut bots = parse(input);

    loop {
        for i in 0..bots.len() {
            let mut bot = bots[&i].clone();
            if bot.values.len() < 2 {
                continue;
            }

            assert_eq!(bot.values.len(), 2);
            bot.values.sort();

            if bot.values == [17, 61] {
                return bot.id;
            }

            for (v, dst) in bot.values.iter().zip(&[bot.low_to, bot.high_to]) {
                match dst {
                    Dest::Bot(id) => {
                        bots.get_mut(id).unwrap().values.push(*v);
                    }
                    Dest::Output(_) => {}
                }
            }

            bots.get_mut(&i).unwrap().values.clear();
        }
    }
}

pub fn part2(input: &str) -> i64 {
    let mut bots = parse(input);
    let mut outputs: HashMap<_, i64> = HashMap::new();

    loop {
        for i in 0..bots.len() {
            let mut bot = bots[&i].clone();
            if bot.values.len() < 2 {
                continue;
            }

            assert_eq!(bot.values.len(), 2);
            bot.values.sort();

            for (v, dst) in bot.values.iter().zip(&[bot.low_to, bot.high_to]) {
                match dst {
                    Dest::Bot(id) => {
                        bots.get_mut(id).unwrap().values.push(*v);
                    }
                    Dest::Output(id) => {
                        *outputs.entry(*id).or_default() = *v;
                    }
                }
            }

            bots.get_mut(&i).unwrap().values.clear();
        }

        if let Some(res) = (0..=2).map(|id| outputs.get(&id)).product() {
            return res;
        }
    }
}

fn parse(input: &str) -> HashMap<BotId, Bot> {
    let mut bots: HashMap<_, Bot> = HashMap::new();

    for l in input.lines() {
        if l.starts_with("bot ") {
            let mut parts = l.split_ascii_whitespace();
            let id = parts.nth(1).unwrap().parse().unwrap();

            let low_to_dest = parts.nth(3).unwrap();
            let low_to = parts.nth(0).unwrap().parse().unwrap();
            let high_to_dest = parts.nth(3).unwrap();
            let high_to = parts.nth(0).unwrap().parse().unwrap();

            let low_to = match low_to_dest {
                "bot" => Dest::Bot(low_to),
                "output" => Dest::Output(low_to),
                _ => unreachable!(),
            };
            let high_to = match high_to_dest {
                "bot" => Dest::Bot(high_to),
                "output" => Dest::Output(high_to),
                _ => unreachable!(),
            };

            bots.entry(id)
                .and_modify(|b| {
                    b.low_to = low_to;
                    b.high_to = high_to;
                })
                .or_insert(Bot {
                    id,
                    low_to,
                    high_to,
                    values: vec![],
                });

            continue;
        }

        if l.starts_with("value ") {
            let mut parts = l.split_ascii_whitespace();
            let value = parts.nth(1).unwrap().parse().unwrap();
            let id = parts.nth(3).unwrap().parse().unwrap();

            bots.entry(id)
                .and_modify(|b| {
                    b.values.push(value);
                })
                .or_insert(Bot {
                    id,
                    low_to: Dest::Bot(0),
                    high_to: Dest::Bot(0),
                    values: vec![value],
                });

            continue;
        }

        unreachable!();
    }

    bots
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day10.txt")), 116);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day10.txt")), 23903);
    }
}

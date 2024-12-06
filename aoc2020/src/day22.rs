use std::collections::{HashSet, VecDeque};

pub fn part1(input: &str) -> usize {
    let (mut p1, mut p2) = parse(input);

    while !p1.is_empty() && !p2.is_empty() {
        let c1 = p1.pop_front().unwrap();
        let c2 = p2.pop_front().unwrap();

        if c1 > c2 {
            p1.push_back(c1);
            p1.push_back(c2);
        } else {
            p2.push_back(c2);
            p2.push_back(c1);
        }
    }

    if p1.is_empty() {
        score(&p2)
    } else {
        score(&p1)
    }
}

pub fn part2(input: &str) -> usize {
    let (mut p1, mut p2) = parse(input);

    let p1_won = rec_play(&mut p1, &mut p2);

    if p1_won {
        score(&p1)
    } else {
        score(&p2)
    }
}

fn rec_play(p1: &mut VecDeque<usize>, p2: &mut VecDeque<usize>) -> bool {
    let mut seen = HashSet::new();

    while !p1.is_empty() && !p2.is_empty() {
        if !seen.insert((p1.clone(), p2.clone())) {
            return true;
        }

        let c1 = p1.pop_front().unwrap();
        let c2 = p2.pop_front().unwrap();

        let p1_won;
        if p1.len() >= c1 && p2.len() >= c2 {
            p1_won = rec_play(
                &mut p1.iter().take(c1).copied().collect(),
                &mut p2.iter().take(c2).copied().collect(),
            );
        } else {
            p1_won = c1 > c2;
        }

        if p1_won {
            p1.push_back(c1);
            p1.push_back(c2);
        } else {
            p2.push_back(c2);
            p2.push_back(c1);
        }
    }

    p2.is_empty()
}

fn score(deck: &VecDeque<usize>) -> usize {
    deck.iter()
        .rev()
        .enumerate()
        .map(|(i, c)| (i + 1) * c)
        .sum()
}

fn parse(input: &str) -> (VecDeque<usize>, VecDeque<usize>) {
    let mut p1 = VecDeque::new();
    let mut p2 = VecDeque::new();

    {
        let mut p = &mut p1;
        for l in input.lines() {
            if l.is_empty() {
                continue;
            }

            if l == "Player 1:" {
                p = &mut p1;
                continue;
            }

            if l == "Player 2:" {
                p = &mut p2;
                continue;
            }

            p.push_back(l.parse().unwrap());
        }
    }

    (p1, p2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(32102, part1(include_str!("../input/day22.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(34173, part2(include_str!("../input/day22.txt")));
    }
}

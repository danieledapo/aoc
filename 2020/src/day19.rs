use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
enum Rule {
    Char(char),
    Seq(Vec<Vec<usize>>),
}

pub fn part1(input: &str) -> usize {
    let (rules, msgs) = parse(input);

    msgs.iter()
        .filter(|l| {
            let l = l.as_bytes();
            check(&rules, 0, l, 0).contains(&l.len())
        })
        .count()
}

pub fn part2(input: &str) -> usize {
    let (mut rules, msgs) = parse(input);

    rules.insert(8, Rule::Seq(vec![vec![42], vec![42, 8]]));
    rules.insert(11, Rule::Seq(vec![vec![42, 31], vec![42, 11, 31]]));

    msgs.iter()
        .filter(|l| {
            let l = l.as_bytes();
            check(&rules, 0, l, 0).contains(&l.len())
        })
        .count()
}

fn check(rules: &HashMap<usize, Rule>, rule: usize, l: &[u8], start: usize) -> HashSet<usize> {
    match &rules[&rule] {
        Rule::Char(c) => {
            let mut set = HashSet::new();
            if start < l.len() && *c == l[start] as char {
                set.insert(start + 1);
            }
            set
        }
        Rule::Seq(seqs) => {
            let mut endings = HashSet::new();
            for sr in seqs {
                let mut buffer = HashSet::new();
                buffer.insert(start);

                for part in sr {
                    buffer = buffer
                        .into_iter()
                        .flat_map(|loc| check(rules, *part, l, loc))
                        .collect();
                }
                endings.extend(&buffer);
            }
            endings
        }
    }
}

fn parse(input: &str) -> (HashMap<usize, Rule>, Vec<&str>) {
    let mut lines = input.lines();

    let rules = lines
        .by_ref()
        .take_while(|l| !l.is_empty())
        .map(|l| {
            let mut parts = l.split(": ");
            let n = parts.next().unwrap().parse::<usize>().unwrap();

            let v = parts.next().unwrap();
            if v.starts_with('"') {
                let c = v.trim_matches('"').chars().next().unwrap();
                (n, Rule::Char(c))
            } else {
                let seqs = v
                    .split(" | ")
                    .map(|sns| sns.split_whitespace().map(|n| n.parse().unwrap()).collect())
                    .collect();

                (n, Rule::Seq(seqs))
            }
        })
        .collect();

    let matches = lines.collect();

    (rules, matches)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(210, part1(include_str!("../input/day19.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(422, part2(include_str!("../input/day19.txt")));
    }
}

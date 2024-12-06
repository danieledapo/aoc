use std::collections::HashMap;

#[derive(Debug)]
pub struct Polymer {
    pub template: Vec<char>,
    pub rules: HashMap<(char, char), char>,
}

pub fn part1(input: &str) -> usize {
    let polymer = parse(input);
    polymer.evolve(10)
}

pub fn part2(input: &str) -> usize {
    let polymer = parse(input);
    polymer.evolve(40)
}

impl Polymer {
    pub fn evolve(&self, n: usize) -> usize {
        let mut pairs: HashMap<(char, char), usize> = HashMap::new();
        for w in self.template.windows(2) {
            *pairs.entry((w[0], w[1])).or_default() += 1;
        }

        let mut letters: HashMap<char, usize> = HashMap::new();
        for &c in self.template.iter() {
            *letters.entry(c).or_default() += 1;
        }

        for _ in 0..n {
            let old_pairs = pairs.clone();

            for (&(a, b), &c) in self.rules.iter() {
                let count = old_pairs.get(&(a, b)).cloned().unwrap_or_default();

                *pairs.entry((a, b)).or_default() -= count;
                *pairs.entry((a, c)).or_default() += count;
                *pairs.entry((c, b)).or_default() += count;

                *letters.entry(c).or_default() += count;
            }
        }

        let max = *letters.values().max().unwrap();
        let min = *letters.values().min().unwrap();

        max - min
    }
}

pub fn parse(input: &str) -> Polymer {
    let mut lines = input.lines();

    let template = lines.next().unwrap().chars().collect();

    lines.next().unwrap(); // empty line

    let rules = lines
        .map(|l| {
            let (a, b) = l.split_once(" -> ").unwrap();

            let mut c = a.chars();
            let aa = c.next().unwrap();
            let ab = c.next().unwrap();
            let b = b.chars().next().unwrap();
            ((aa, ab), b)
        })
        .collect();

    Polymer { template, rules }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2947, part1(include_str!("../input/day14.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(3232426226464, part2(include_str!("../input/day14.txt")));
    }
}

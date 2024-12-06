use std::collections::HashSet;

pub fn part1(input: &str) -> i64 {
    parse(input)
        .map(|s| {
            let (a, b) = s.split_at(s.len() / 2);

            let aa: HashSet<_> = a.iter().collect();
            let bb: HashSet<_> = b.iter().collect();

            aa.intersection(&bb).map(|a| **a).sum::<i64>()
        })
        .sum()
}

pub fn part2(input: &str) -> i64 {
    let data = parse(input).collect::<Vec<_>>();

    data.chunks_exact(3)
        .map(|d| {
            let [a, b, c] = [&d[0], &d[1], &d[2]];

            let a: HashSet<_> = a.iter().collect();
            let b: HashSet<_> = b.iter().collect();
            let c: HashSet<_> = c.iter().collect();

            HashSet::from_iter(a.intersection(&b).copied())
                .intersection(&c)
                .map(|a| **a)
                .sum::<i64>()
        })
        .sum()
}

fn parse(input: &str) -> impl Iterator<Item = Vec<i64>> + '_ {
    input.lines().map(|l| {
        l.as_bytes()
            .iter()
            .map(|b| {
                i64::from(if b.is_ascii_lowercase() {
                    b - b'a' + 1
                } else {
                    b - b'A' + 27
                })
            })
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(8252, part1(include_str!("../input/day3.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(2828, part2(include_str!("../input/day3.txt")));
    }
}

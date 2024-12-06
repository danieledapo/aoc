use std::{ops::RangeInclusive, str::FromStr};

type R = RangeInclusive<i64>;

pub fn part1(input: &str) -> usize {
    parse(input)
        .filter(|(a, b)| {
            (a.start() <= b.start() && a.end() >= b.end())
                || (b.start() <= a.start() && b.end() >= a.end())
        })
        .count()
}

pub fn part2(input: &str) -> usize {
    parse(input)
        .filter(|(a, b)| {
            a.contains(b.start())
                || a.contains(b.end())
                || b.contains(a.start())
                || b.contains(a.end())
        })
        .count()
}

fn parse(input: &str) -> impl Iterator<Item = (R, R)> + '_ {
    input.lines().map(|l| {
        let (a, b) = l.split_once(',').unwrap();
        let (aa, ab) = a.split_once('-').unwrap();
        let (ba, bb) = b.split_once('-').unwrap();

        (
            i64::from_str(aa).unwrap()..=i64::from_str(ab).unwrap(),
            i64::from_str(ba).unwrap()..=i64::from_str(bb).unwrap(),
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(651, part1(include_str!("../input/day4.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(956, part2(include_str!("../input/day4.txt")));
    }
}

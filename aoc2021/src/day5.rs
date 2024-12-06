use std::collections::HashMap;

struct Line {
    x0: i64,
    y0: i64,
    x1: i64,
    y1: i64,
}

pub fn part1(input: &str) -> usize {
    let mut freqs: HashMap<(i64, i64), usize> = HashMap::new();

    for l in parse(input).filter(Line::is_ortho) {
        for p in l.points() {
            *freqs.entry(p).or_default() += 1;
        }
    }

    freqs.values().filter(|n| **n > 1).count()
}

pub fn part2(input: &str) -> usize {
    let mut freqs: HashMap<(i64, i64), usize> = HashMap::new();

    for l in parse(input) {
        for p in l.points() {
            *freqs.entry(p).or_default() += 1;
        }
    }

    freqs.values().filter(|n| **n > 1).count()
}

impl Line {
    fn is_ortho(&self) -> bool {
        self.x0 == self.x1 || self.y0 == self.y1
    }

    fn points(&self) -> impl Iterator<Item = (i64, i64)> + '_ {
        let (mut x, mut y) = (self.x0, self.y0);
        let dx = (self.x1 - self.x0).signum();
        let dy = (self.y1 - self.y0).signum();

        Some((x, y)).into_iter().chain(std::iter::from_fn(move || {
            if x == self.x1 && y == self.y1 {
                return None;
            }

            x += dx;
            y += dy;

            Some((x, y))
        }))
    }
}

fn parse(input: &str) -> impl Iterator<Item = Line> + '_ {
    input.lines().map(|l| {
        let (a, b) = l.split_once(" -> ").unwrap();
        let (x0, y0) = a.split_once(',').unwrap();
        let (x1, y1) = b.split_once(',').unwrap();

        Line {
            x0: x0.parse().unwrap(),
            y0: y0.parse().unwrap(),
            x1: x1.parse().unwrap(),
            y1: y1.parse().unwrap(),
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(6267, part1(include_str!("../input/day5.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(20196, part2(include_str!("../input/day5.txt")));
    }
}

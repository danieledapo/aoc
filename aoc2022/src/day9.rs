use std::collections::HashSet;

pub fn part1(input: &str) -> usize {
    simulate(input, 2)
}

pub fn part2(input: &str) -> usize {
    simulate(input, 10)
}

fn simulate(input: &str, nknots: usize) -> usize {
    let mut seen = HashSet::new();
    let mut knots = vec![(0, 0); nknots];

    for (dx, dy, n) in parse(input) {
        for _ in 0..n {
            knots[0] = (knots[0].0 + dx, knots[0].1 + dy);

            for i in 1..knots.len() {
                if i32::abs(knots[i - 1].0 - knots[i].0) > 1
                    || i32::abs(knots[i - 1].1 - knots[i].1) > 1
                {
                    knots[i] = (
                        knots[i].0 + (knots[i - 1].0 - knots[i].0).signum(),
                        knots[i].1 + (knots[i - 1].1 - knots[i].1).signum(),
                    );
                }
            }
            seen.insert(knots.last().copied().unwrap());
        }
    }

    seen.len()
}

fn parse(input: &str) -> impl Iterator<Item = (i32, i32, u32)> + '_ {
    input.lines().map(|l| {
        let (d, a) = l.split_once(' ').unwrap();

        let n: u32 = a.parse().unwrap();

        match d {
            "R" => (1, 0, n),
            "L" => (-1, 0, n),
            "U" => (0, 1, n),
            "D" => (0, -1, n),
            _ => unreachable!(),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(6197, part1(include_str!("../input/day9.txt"),));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(2562, part2(include_str!("../input/day9.txt"),));
    }
}

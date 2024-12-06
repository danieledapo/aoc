pub fn part1(input: &str) -> i64 {
    let crabs = parse(input);
    let max_p = *crabs.iter().max().unwrap();

    let mut best = 0;
    for p in 0..max_p {
        let mut e = 0;
        for cp in &crabs {
            e += i64::abs(*cp - p);
            if p > 0 && e >= best {
                break;
            }
        }

        if p == 0 || e < best {
            best = e;
        }
    }

    best
}

pub fn part2(input: &str) -> i64 {
    let crabs = parse(input);
    let max_p = *crabs.iter().max().unwrap();

    let mut best = 0;
    for p in 0..max_p {
        let mut e = 0;
        for cp in &crabs {
            let steps = i64::abs(*cp - p);
            e += steps * (steps + 1) / 2;
            if p > 0 && e >= best {
                break;
            }
        }

        if p == 0 || e < best {
            best = e;
        }
    }

    best
}

fn parse(input: &str) -> Vec<i64> {
    input
        .trim()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(336701, part1(include_str!("../input/day7.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(95167302, part2(include_str!("../input/day7.txt")));
    }
}

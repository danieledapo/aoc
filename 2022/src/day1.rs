use std::cmp::Reverse;

pub fn part1(input: &str) -> i64 {
    parse(input)[0]
}

pub fn part2(input: &str) -> i64 {
    let n = parse(input);
    n[0] + n[1] + n[2]
}

fn parse(input: &str) -> Vec<i64> {
    let mut res = vec![];
    let mut curr = 0;
    for l in input.lines() {
        if l.is_empty() {
            res.push(curr);
            curr = 0;
            continue;
        }

        curr += l.parse::<i64>().unwrap();
    }

    res.push(curr);
    res.sort_unstable_by_key(|n| Reverse(*n));
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(69206, part1(include_str!("../input/day1.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(197400, part2(include_str!("../input/day1.txt")));
    }
}

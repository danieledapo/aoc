use std::collections::HashSet;

pub fn part1(input: &str) -> i64 {
    input.lines().map(|i| i.parse::<i64>().unwrap()).sum()
}

pub fn part2(input: &str) -> i64 {
    let mut freq = 0;
    let mut already_seen = HashSet::new();

    let freq_offs = input.lines().map(|i| i.parse::<i64>().unwrap()).cycle();

    for off in freq_offs {
        if already_seen.insert(freq) {
            freq += off;
        } else {
            break;
        }
    }

    freq
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(525, part1(include_str!("../input/day1.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(75749, part2(include_str!("../input/day1.txt")));
    }
}

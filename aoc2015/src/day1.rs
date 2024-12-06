pub fn part1(input: &str) -> i32 {
    input
        .trim()
        .chars()
        .map(|c| if c == '(' { 1 } else { -1 })
        .sum()
}

pub fn part2(input: &str) -> usize {
    1 + input
        .trim()
        .chars()
        .scan(0, |s, c| {
            *s += if c == '(' { 1 } else { -1 };

            Some(*s)
        })
        .position(|s| s < 0)
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(74, part1(include_str!("../input/day1.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(1795, part2(include_str!("../input/day1.txt")));
    }
}

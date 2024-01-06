use std::collections::HashSet;

pub type Stack = Vec<Vec<u8>>;

pub fn part1(input: &str) -> usize {
    go(input, 4)
}

pub fn part2(input: &str) -> usize {
    go(input, 14)
}

fn go(input: &str, n: usize) -> usize {
    let b = input.trim().as_bytes();

    b.windows(n)
        .enumerate()
        .find(|(_, a)| {
            let seen: HashSet<_> = a.iter().copied().collect();
            seen.len() == n
        })
        .unwrap()
        .0
        + n
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(1155, part1(include_str!("../input/day6.txt"),));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(2789, part2(include_str!("../input/day6.txt"),));
    }
}

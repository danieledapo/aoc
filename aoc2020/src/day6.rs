use std::collections::HashSet;

pub fn part1(input: &str) -> usize {
    let mut yes = HashSet::new();

    let mut c = 0;
    for l in input.lines() {
        if l.is_empty() {
            c += yes.len();
            yes.clear();
            continue;
        }

        yes.extend(l.chars());
    }

    if !yes.is_empty() {
        c += yes.len();
    }

    c
}

pub fn part2(input: &str) -> usize {
    let mut yes = HashSet::new();
    let mut in_group = false;

    let mut c = 0;
    for l in input.lines() {
        if l.is_empty() {
            c += yes.len();
            yes.clear();
            in_group = false;
            continue;
        }

        let chars = l.chars().collect();
        if !in_group {
            yes = chars;
            in_group = true;
        } else {
            yes = yes.intersection(&chars).copied().collect();
        }
    }

    if !yes.is_empty() {
        c += yes.len();
    }

    c
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(6625, part1(include_str!("../input/day6.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(3360, part2(include_str!("../input/day6.txt")));
    }
}

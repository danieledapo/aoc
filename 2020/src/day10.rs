use std::collections::{BTreeSet, HashMap};

pub fn part1(input: &str) -> u32 {
    let mut count_diff1 = 0;
    let mut count_diff3 = 1; // include the final device

    let mut prev = 0;

    for &n in &parse(input) {
        let d = n - prev;

        if d == 1 {
            count_diff1 += 1;
        } else if d == 3 {
            count_diff3 += 1;
        }

        prev = n;
    }

    count_diff1 * count_diff3
}

pub fn part2(input: &str) -> u64 {
    fn go(cache: &mut HashMap<u32, u64>, nums: &BTreeSet<u32>, n: u32, max: u32) -> u64 {
        if let Some(c) = cache.get(&n) {
            return *c;
        }

        let mut c = if n + 3 >= max { 1 } else { 0 };
        for d in 1..=3 {
            if nums.contains(&(n + d)) {
                c += go(cache, &nums, n + d, max);
            }
        }

        cache.insert(n, c);
        c
    }

    let nums = parse(input);
    go(
        &mut HashMap::new(),
        &nums,
        0,
        *nums.iter().next_back().unwrap() + 3,
    )
}

fn parse(input: &str) -> BTreeSet<u32> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2112, part1(include_str!("../input/day10.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(3022415986688, part2(include_str!("../input/day10.txt")));
    }
}

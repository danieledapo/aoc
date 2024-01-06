use std::collections::{hash_map::Entry, HashMap};

pub fn part1(nums: &[usize]) -> usize {
    solve(nums, 2020)
}

pub fn part2(nums: &[usize]) -> usize {
    // fast enough in release builds...
    solve(nums, 30000000)
}

fn solve(nums: &[usize], n: usize) -> usize {
    let mut seen = HashMap::new();
    let mut last = 0;
    for (i, &n) in nums.iter().enumerate().take(n) {
        seen.insert(n, i + 1);
        last = n;
    }

    for t in nums.len()..n {
        match seen.entry(last) {
            Entry::Vacant(v) => {
                v.insert(t);
                last = 0;
            }
            Entry::Occupied(mut o) => {
                let age = t - *o.get();
                *o.get_mut() = t;
                last = age;
            }
        }
    }

    last
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(706, part1(&[0, 6, 1, 7, 2, 19, 20]));
    }

    #[test]
    fn test_part2() {
        assert_eq!(19331, part2(&[0, 6, 1, 7, 2, 19, 20]));
    }
}

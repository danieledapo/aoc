use std::collections::HashSet;

pub fn part1(input: &str) -> u64 {
    let nums = parse(input).collect::<Vec<_>>();
    let preamble_elems = 25;

    let mut preamble = nums
        .iter()
        .take(preamble_elems)
        .copied()
        .collect::<HashSet<_>>();

    for (i, &n) in nums.iter().enumerate().skip(preamble_elems) {
        let found = preamble
            .iter()
            .any(|&pn| pn <= n && preamble.contains(&(n - pn)));

        if !found {
            return n;
        }

        preamble.remove(&(nums[i - preamble_elems]));
        preamble.insert(n);
    }

    unreachable!()
}

pub fn part2(input: &str) -> u64 {
    let target = 14144619;

    let nums = parse(input).collect::<Vec<_>>();

    for (i, &n) in nums.iter().enumerate() {
        let mut max = n;
        let mut min = n;
        let mut sum = n;

        for &nn in nums.iter().skip(i + 1) {
            max = max.max(nn);
            min = min.min(nn);
            sum += nn;

            if sum == target {
                return min + max;
            }

            if sum > target {
                break;
            }
        }
    }

    unreachable!()
}

fn parse(input: &str) -> impl Iterator<Item = u64> + '_ {
    input.lines().map(|l| l.parse::<u64>().unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(14144619, part1(include_str!("../input/day9.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1766397, part2(include_str!("../input/day9.txt")));
    }
}

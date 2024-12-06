use std::cmp::{Ordering, Reverse};

pub fn part1(input: &str) -> usize {
    let mut buckets = input
        .trim()
        .lines()
        .map(|l| l.parse::<u32>().unwrap())
        .collect::<Vec<_>>();

    buckets.sort_by_key(|c| Reverse(*c));

    let arrangements = find_arrangements(&buckets, 150);
    arrangements.len()
}

pub fn part2(input: &str) -> usize {
    let mut buckets = input
        .trim()
        .lines()
        .map(|l| l.parse::<u32>().unwrap())
        .collect::<Vec<_>>();

    buckets.sort_by_key(|c| Reverse(*c));

    let arrangements = find_arrangements(&buckets, 150);

    let min_containers_len = arrangements.iter().map(|a| a.len()).min().unwrap();

    arrangements
        .iter()
        .filter(|a| a.len() == min_containers_len)
        .count()
}

fn find_arrangements(buckets: &[u32], sum: u32) -> Vec<Vec<u32>> {
    if buckets.is_empty() {
        return vec![];
    }

    let mut res = vec![];

    match buckets[0].cmp(&sum) {
        Ordering::Equal => {
            res.push(vec![sum]);
        }
        Ordering::Less => {
            res = find_arrangements(&buckets[1..], sum - buckets[0]);
            for s in &mut res {
                s.push(buckets[0]);
            }
        }
        Ordering::Greater => {}
    }

    res.extend_from_slice(&find_arrangements(&buckets[1..], sum));

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(1304, part1(include_str!("../input/day17.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(18, part2(include_str!("../input/day17.txt")));
    }
}

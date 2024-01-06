use std::cmp::{Ordering, Reverse};

pub fn part1(input: &str) -> u128 {
    balance(input, 3)
}

pub fn part2(input: &str) -> u128 {
    balance(input, 4)
}

fn balance(input: &str, div: u128) -> u128 {
    let mut weights = input
        .trim()
        .lines()
        .map(|l| l.parse::<u128>().unwrap())
        .collect::<Vec<_>>();

    let sum = weights.iter().sum::<u128>() / div;

    weights.sort_by_key(|c| Reverse(*c));

    // don't bother optimizing this, it's fast enough
    find_arrangements(&weights, sum)
        .into_iter()
        .min_by_key(|a| (a.len(), a.iter().product::<u128>()))
        .unwrap()
        .iter()
        .product()
}

fn find_arrangements(buckets: &[u128], sum: u128) -> Vec<Vec<u128>> {
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
        assert_eq!(10_439_961_859, part1(include_str!("../input/day24.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(72_050_269, part2(include_str!("../input/day24.txt")));
    }
}

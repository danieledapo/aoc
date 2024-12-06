use std::collections::HashMap;

pub fn part1(input: &str) -> String {
    get_counts(input)
        .iter()
        .map(|counts| *counts.iter().max_by_key(|(_, c)| *c).unwrap().0)
        .collect()
}

pub fn part2(input: &str) -> String {
    get_counts(input)
        .iter()
        .map(|counts| *counts.iter().min_by_key(|(_, c)| *c).unwrap().0)
        .collect()
}

fn get_counts(input: &str) -> Vec<HashMap<char, usize>> {
    let mut it = input.lines();

    let mut counts = vec![];
    let update_counts = |l: &str, counts: &mut Vec<HashMap<char, usize>>| {
        for (i, c) in l.chars().enumerate() {
            *counts[i].entry(c).or_insert(0) += 1;
        }
    };

    if let Some(l) = it.next() {
        counts = vec![HashMap::new(); l.chars().count()];
        update_counts(l, &mut counts);
    }

    for l in it {
        update_counts(l, &mut counts);
    }

    counts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day6.txt")), "xhnqpqql");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day6.txt")), "brhailro");
    }
}

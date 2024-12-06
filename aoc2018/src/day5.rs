pub fn part1<'a>(polymer: impl Iterator<Item = &'a u8>) -> usize {
    let mut new_polymer = vec![];

    for unit in polymer {
        if new_polymer.is_empty() || !are_opposite(*unit, new_polymer[new_polymer.len() - 1]) {
            new_polymer.push(*unit);
        } else {
            new_polymer.pop();
        }
    }

    new_polymer.len()
}

pub fn part2(polymer: &[u8]) -> usize {
    (b'a'..b'z')
        .map(|c| part1(polymer.iter().filter(|u| u.to_ascii_lowercase() != c)))
        .min()
        .unwrap()
}

fn are_opposite(c1: u8, c2: u8) -> bool {
    c1 != c2 && c1.to_ascii_lowercase() == c2.to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(10, part1(b"dabAcCaCBAcCcaDA".iter()));
        assert_eq!(9808, part1(include_bytes!("../input/day5.txt").iter()));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(4, part2(b"dabAcCaCBAcCcaDA"));
        assert_eq!(6484, part2(include_bytes!("../input/day5.txt")));
    }

}

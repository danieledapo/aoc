use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Room<'s> {
    name: &'s str,
    sector_id: i64,
    checksum: &'s str,
}

pub fn part1(input: &str) -> i64 {
    parse(input).filter(Room::valid).map(|r| r.sector_id).sum()
}

pub fn part2(input: &str) -> i64 {
    parse(input)
        .find(|r| r.decrypted_name() == "northpole object storage")
        .unwrap()
        .sector_id
}

impl<'a> Room<'a> {
    fn new(l: &'a str) -> Self {
        let mut parts = l.rsplitn(2, '-');

        let info = parts.next().unwrap();
        let name = parts.next().unwrap();

        let mut parts = info.trim_end_matches(']').split('[');
        let sector_id = parts.next().unwrap().parse().unwrap();
        let checksum = parts.next().unwrap();

        Room {
            name,
            sector_id,
            checksum,
        }
    }

    fn valid(&self) -> bool {
        let mut counts: HashMap<_, i64> = HashMap::new();

        for c in self.name.chars().filter(|c| *c != '-') {
            *counts.entry(c).or_default() += 1;
        }

        let mut most_common = counts.into_iter().collect::<Vec<_>>();
        most_common.sort_by_key(|(c, n)| (-n, *c));

        self.checksum
            .chars()
            .zip(most_common.iter().map(|(c, _)| *c))
            .all(|(c1, c2)| c1 == c2)
    }

    fn decrypted_name(&self) -> String {
        self.name
            .chars()
            .map(|c| {
                if c == '-' {
                    return ' ';
                }

                let off = (i64::from(c as u8 - b'a') + self.sector_id) % 26;
                (b'a' + off as u8) as char
            })
            .collect()
    }
}

fn parse(input: &str) -> impl Iterator<Item = Room> {
    input.lines().map(|l| Room::new(l))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day4.txt")), 361724);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day4.txt")), 482);
    }
}

use std::collections::BTreeSet;

pub fn part1(input: &str) -> u32 {
    parse(input).max().unwrap()
}

pub fn part2(input: &str) -> u32 {
    let seats = parse(input).collect::<BTreeSet<_>>();

    for id in &seats {
        if !seats.contains(&(id + 1)) && seats.contains(&(id + 2)) {
            return id + 1;
        }
    }

    unreachable!()
}

fn parse(input: &str) -> impl Iterator<Item = u32> + '_ {
    input.lines().map(|l| {
        let mut ty = 0;
        let mut by = 127;
        let mut lx = 0;
        let mut rx = 7;

        for c in l.chars() {
            match c {
                'F' => by = (ty + by) / 2,
                'B' => ty = (ty + by) / 2 + ((by - ty) & 1),
                'L' => rx = (lx + rx) / 2,
                'R' => lx = (lx + rx) / 2 + ((rx - lx) & 1),
                _ => panic!("bad {}", c),
            }
        }

        ty * 8 + rx
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(861, part1(include_str!("../input/day5.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(633, part2(include_str!("../input/day5.txt")));
    }
}

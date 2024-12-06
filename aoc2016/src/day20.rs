use std::ops::RangeInclusive;

pub fn part1(input: &str) -> u32 {
    let mut blacklist = parse(input).collect::<Vec<_>>();
    blacklist.sort_by_key(|l| *l.start());

    (0..)
        .find(|n| {
            blacklist
                .iter()
                .take_while(|r| r.start() <= n)
                .all(|r| !r.contains(&n))
        })
        .unwrap()
}

pub fn part2(input: &str) -> usize {
    let mut blacklist = parse(input).collect::<Vec<_>>();
    blacklist.sort_by_key(|l| *l.start());

    let mut c = 0;
    let mut start = 0;
    for n in 0..=u32::max_value() {
        let mut blocked = false;
        for (i, r) in blacklist.iter().enumerate().skip(start) {
            if *r.start() > n {
                break;
            }

            if r.contains(&n) {
                blocked = true;
                break;
            }

            if *r.end() < n {
                start = i + 1;
                continue;
            }
        }

        if !blocked {
            c += 1;
        }
    }

    c
}

fn parse(input: &str) -> impl Iterator<Item = RangeInclusive<u32>> + '_ {
    input.lines().map(|l| {
        let mut parts = l.split('-');
        let s = parts.next().unwrap().parse().unwrap();
        let e = parts.next().unwrap().parse().unwrap();

        s..=e
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day20.txt")), 32259706);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day20.txt")), 113);
    }
}

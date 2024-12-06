pub type Stack = Vec<Vec<u8>>;

pub fn part1(input: &str, skip: usize, mut stacks: Stack) -> String {
    for (qty, src, dst) in parse(input, skip) {
        for _ in 0..qty {
            let d = stacks[src - 1].pop().unwrap();
            stacks[dst - 1].push(d);
        }
    }

    stacks.iter().map(|s| *s.last().unwrap() as char).collect()
}

pub fn part2(input: &str, skip: usize, mut stacks: Stack) -> String {
    for (qty, src, dst) in parse(input, skip) {
        let last = stacks[src - 1].len();
        let first = last - qty;

        for i in 0..qty {
            let d = stacks[src - 1][first + i];
            stacks[dst - 1].push(d);
        }

        for _ in 0..qty {
            stacks[src - 1].pop();
        }
    }

    stacks.iter().map(|s| *s.last().unwrap() as char).collect()
}

fn parse(input: &str, skip: usize) -> impl Iterator<Item = (usize, usize, usize)> + '_ {
    input.lines().skip(skip).map(|l| {
        let mut parts = l.split_whitespace();
        let qty = parts.nth(1).unwrap().parse().unwrap();
        let src = parts.nth(1).unwrap().parse().unwrap();
        let dst = parts.nth(1).unwrap().parse().unwrap();

        (qty, src, dst)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(
            "FWSHSPJWM",
            &part1(
                include_str!("../input/day5.txt"),
                10,
                vec![
                    b"SZPDLBFC".to_vec(),
                    b"NVGPHWB".to_vec(),
                    b"FWBJG".to_vec(),
                    b"GJNFLWCS".to_vec(),
                    b"WJLTPMSH".to_vec(),
                    b"BCWGFS".to_vec(),
                    b"HTPMQBW".to_vec(),
                    b"FSWT".to_vec(),
                    b"NCR".to_vec()
                ]
            )
        );
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(
            "PWPWHGFZS",
            &part2(
                include_str!("../input/day5.txt"),
                10,
                vec![
                    b"SZPDLBFC".to_vec(),
                    b"NVGPHWB".to_vec(),
                    b"FWBJG".to_vec(),
                    b"GJNFLWCS".to_vec(),
                    b"WJLTPMSH".to_vec(),
                    b"BCWGFS".to_vec(),
                    b"HTPMQBW".to_vec(),
                    b"FSWT".to_vec(),
                    b"NCR".to_vec()
                ]
            )
        );
    }
}

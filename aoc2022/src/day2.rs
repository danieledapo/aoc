pub fn part1(input: &str) -> i64 {
    score(parse(input))
}

pub fn part2(input: &str) -> i64 {
    score(parse(input).map(|(a, b)| {
        if b == 0 {
            (a, (a - 1 + 3) % 3)
        } else if b == 1 {
            (a, a)
        } else {
            (a, (a + 1) % 3)
        }
    }))
}

fn score(it: impl Iterator<Item = (i64, i64)>) -> i64 {
    it.map(|(a, b)| {
        let outcome = if a == b {
            3
        } else if (a + 1) % 3 == b {
            6
        } else {
            0
        };

        b + 1 + outcome
    })
    .sum()
}

fn parse(input: &str) -> impl Iterator<Item = (i64, i64)> + '_ {
    input.lines().map(|l| {
        let (a, b) = l.split_once(' ').unwrap();
        let a = i64::from(a.as_bytes()[0] - b'A');
        let b = i64::from(b.as_bytes()[0] - b'X');

        (a, b)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(11666, part1(include_str!("../input/day2.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(12767, part2(include_str!("../input/day2.txt")));
    }
}

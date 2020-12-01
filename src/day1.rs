pub fn part1(input: &str) -> i64 {
    let nums: Vec<_> = input.lines().map(|l| l.parse::<i64>().unwrap()).collect();

    for (i, n) in nums.iter().enumerate() {
        for nn in nums.iter().skip(i + 1) {
            if n + nn == 2020 {
                return n * nn;
            }
        }
    }

    unreachable!()
}

pub fn part2(input: &str) -> i64 {
    let nums: Vec<_> = input.lines().map(|l| l.parse::<i64>().unwrap()).collect();

    for (i, n) in nums.iter().enumerate() {
        for (j, nn) in nums.iter().enumerate().skip(i + 1) {
            for nnn in nums.iter().skip(j + 1) {
                if n + nn + nnn == 2020 {
                    return n * nn * nnn;
                }
            }
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day1.txt")), 224436);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day1.txt")), 303394260);
    }
}

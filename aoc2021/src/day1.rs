pub fn part1(input: &str) -> usize {
    let mut lines = input.lines().map(|l| l.parse::<i64>().unwrap());

    let mut count = 0;
    let mut prev = lines.next().unwrap();

    for n in lines {
        if n > prev {
            count += 1;
        }
        prev = n;
    }

    count
}

pub fn part2(input: &str) -> usize {
    let mut count = 0;

    let nums: Vec<i64> = input.lines().map(|l| l.parse().unwrap()).collect();
    for i in 0..nums.len().saturating_sub(3) {
        let a = nums[i] + nums[i + 1] + nums[i + 2];
        let b = nums[i + 1] + nums[i + 2] + nums[i + 3];
        if b > a {
            count += 1;
        }
    }

    count
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(1298, part1(include_str!("../input/day1.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1248, part2(include_str!("../input/day1.txt")));
    }
}

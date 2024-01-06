use std::ops::RangeBounds;

pub fn part1(start: u32, end: u32) -> usize {
    (start..end).filter(|n| meets_requirements(*n, 2..)).count()
}

pub fn part2(start: u32, end: u32) -> usize {
    (start..end)
        .filter(|n| meets_requirements(*n, 2..=2))
        .count()
}

fn meets_requirements(mut n: u32, same_digit_count_range: impl RangeBounds<u32>) -> bool {
    let mut last_d = 11;
    let mut same_digit_count = 1;
    let mut has_consecutive = false;

    loop {
        let d = n % 10;
        n /= 10;

        // if first digit skip any check with last_d as there wasn't a last digit
        if last_d < 10 {
            if d > last_d {
                return false;
            }

            if d == last_d {
                same_digit_count += 1;
                continue;
            }

            if same_digit_count_range.contains(&same_digit_count) {
                has_consecutive = true;
            }
            same_digit_count = 1;
        }

        last_d = d;

        if n == 0 {
            break;
        }
    }

    has_consecutive || same_digit_count_range.contains(&same_digit_count)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(168630, 718098), 1686);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(168630, 718098), 1145);
    }
}

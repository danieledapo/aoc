pub fn part1(input: &str) -> Vec<i32> {
    let mut digits = parse(input);
    let mut new_digits = vec![0; digits.len()];

    for _ in 0..100 {
        for (i, d) in new_digits.iter_mut().enumerate() {
            let repeats = i + 1;

            let mut s = 0;
            let mut w = 1;
            let mut di = repeats - 1;

            while di < digits.len() {
                for d in digits.iter().skip(di).take(repeats) {
                    s += d * w;
                }

                di += repeats * 2;
                w = -w;
            }

            *d = s.abs() % 10;
        }

        std::mem::swap(&mut digits, &mut new_digits);
    }

    digits.iter().take(8).copied().collect()
}

pub fn part2(input: &str) -> Vec<i32> {
    unimplemented!()
}

fn parse(input: &str) -> Vec<i32> {
    input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(
            part1(include_str!("../input/day16.txt")),
            vec![3, 0, 3, 7, 9, 5, 8, 5]
        );
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day16.txt")), vec![]);
    }
}

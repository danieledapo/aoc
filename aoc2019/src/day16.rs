pub fn part1(input: &str) -> Vec<u8> {
    let mut digits = parse(input);
    let mut new_digits = vec![0; digits.len()];

    for _ in 0..100 {
        for (i, d) in new_digits.iter_mut().enumerate() {
            let repeats = i + 1;

            let mut s: i32 = 0;
            let mut w = 1;
            let mut di = repeats - 1;

            while di < digits.len() {
                for d in digits.iter().skip(di).take(repeats) {
                    s += i32::from(*d) * w;
                }

                di += repeats * 2;
                w = -w;
            }

            *d = (s.abs() % 10) as u8;
        }

        std::mem::swap(&mut digits, &mut new_digits);
    }

    digits.iter().take(8).copied().collect()
}

pub fn part2(input: &str) -> Vec<u8> {
    use std::iter::repeat;

    let mut digits = parse(input);

    let offset: usize = digits
        .iter()
        .take(7)
        .rev()
        .enumerate()
        .map(|(i, d)| *d as usize * 10_usize.pow(i as u32))
        .sum();

    digits = repeat(digits).take(10_000).flatten().skip(offset).collect();

    let mut new_digits = vec![0; digits.len()];

    for _ in 0..100 {
        let mut s: i32 = digits.iter().map(|n| i32::from(*n)).sum();
        for (o, i) in new_digits.iter_mut().zip(digits.iter()) {
            *o = (((s % 10) + 10) % 10) as u8;
            s -= i32::from(*i);
        }

        std::mem::swap(&mut digits, &mut new_digits);
    }

    digits.iter().take(8).copied().collect()
}

fn parse(input: &str) -> Vec<u8> {
    input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
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
        assert_eq!(
            part2(include_str!("../input/day16.txt")),
            vec![2, 2, 8, 0, 8, 9, 3, 1]
        );
    }
}

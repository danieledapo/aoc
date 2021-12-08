use itertools::Itertools;

struct Entry<'a> {
    patterns: Vec<&'a str>,
    digits: Vec<&'a str>,
}

pub fn part1(input: &str) -> usize {
    parse(input)
        .flat_map(|e| e.digits)
        .filter(|d| [2, 4, 3, 7].contains(&d.len()))
        .count()
}

pub fn part2(input: &str) -> usize {
    parse(input).map(|e| e.decode()).sum()
}

fn parse(input: &str) -> impl Iterator<Item = Entry> + '_ {
    input.lines().map(|l| {
        let (patterns, digits) = l.split_once(" | ").unwrap();
        Entry {
            patterns: patterns.split_whitespace().collect(),
            digits: digits.split_whitespace().collect(),
        }
    })
}

impl Entry<'_> {
    fn decode(&self) -> usize {
        const DIGITS: [&str; 10] = [
            "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg",
        ];

        for segment in "abcdefg".chars().permutations(7) {
            let translate = |s: &str| -> String {
                s.chars()
                    .map(|b| segment[(b as u8 - 'a' as u8) as usize])
                    .sorted()
                    .collect()
            };

            let mut invalid_digit = false;
            for pattern in &self.patterns {
                let t = translate(*pattern);
                if DIGITS.iter().find(|l| **l == &t).is_none() {
                    invalid_digit = true;
                    break;
                }
            }
            if invalid_digit {
                continue;
            }

            return self.digits.iter().fold(0, |digits, out| {
                let translated = translate(out);

                let n = DIGITS.iter().position(|v| v == &translated).unwrap();

                digits * 10 + n
            });
        }

        unreachable!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(479, part1(include_str!("../input/day8.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1041746, part2(include_str!("../input/day8.txt")));
    }
}

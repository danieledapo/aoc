pub fn part1(input: &str) -> usize {
    fn is_nice(w: &str) -> bool {
        let mut num_vowels = 0;
        let mut last_char = ' ';
        let mut has_duplicates = false;

        for c in w.chars() {
            if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' {
                num_vowels += 1;
            }

            if c == last_char {
                has_duplicates = true;
            }

            if (last_char == 'a' && c == 'b')
                || (last_char == 'c' && c == 'd')
                || (last_char == 'p' && c == 'q')
                || (last_char == 'x' && c == 'y')
            {
                return false;
            }

            last_char = c;
        }

        num_vowels >= 3 && has_duplicates
    }

    input.trim().lines().filter(|l| is_nice(l)).count()
}

pub fn part2(input: &str) -> usize {
    input.trim().lines().filter(|l| is_nice(l)).count()
}

fn is_nice(w: &str) -> bool {
    let chars = w.chars().collect::<Vec<_>>();

    let mut has_repeated_char = false;

    for i in 0..chars.len() - 2 {
        if chars[i] == chars[i + 2] {
            has_repeated_char = true;
            break;
        }
    }

    if !has_repeated_char {
        return false;
    }

    for i in 0..w.len() - 1 {
        let p = (chars[i], chars[i + 1]);

        for j in i + 2..w.len() - 1 {
            if p == (chars[j], chars[j + 1]) {
                return true;
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(238, part1(include_str!("../input/day5.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(69, part2(include_str!("../input/day5.txt")));
    }
}

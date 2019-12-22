use std::collections::HashSet;

pub fn part1(input: &str) -> usize {
    input.lines().filter(|l| supports_tls(*l)).count()
}

pub fn part2(input: &str) -> usize {
    input.lines().filter(|l| supports_ssl(*l)).count()
}

fn supports_tls(l: &str) -> bool {
    let l = l.as_bytes();

    let mut has_abba = false;
    let mut inside_brackets = false;
    for i in 0..l.len().saturating_sub(3) {
        if l[i] == b'[' {
            inside_brackets = true;
            continue;
        }

        if l[i] == b']' {
            inside_brackets = false;
            continue;
        }

        if l[i] == l[i + 1] {
            continue;
        }

        if l[i] == l[i + 3] && l[i + 1] == l[i + 2] {
            has_abba = true;

            if inside_brackets {
                return false;
            }
        }
    }

    has_abba
}

fn supports_ssl(l: &str) -> bool {
    let l = l.as_bytes();

    let mut seqs = HashSet::new();

    let mut inside_brackets = false;
    for i in 0..l.len().saturating_sub(2) {
        if l[i] == b'[' {
            inside_brackets = true;
            continue;
        }

        if l[i] == b']' {
            inside_brackets = false;
            continue;
        }

        if l[i] == l[i + 1] {
            continue;
        }

        if l[i] == l[i + 2] {
            let seq = [l[i], l[i + 1], l[i + 2]];

            if seqs.contains(&([seq[1], seq[0], seq[1]], !inside_brackets)) {
                return true;
            }

            seqs.insert((seq, inside_brackets));
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day7.txt")), 110);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day7.txt")), 242);
    }
}

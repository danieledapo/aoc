pub fn part1(input: &str) -> u64 {
    input.lines().map(corrupted_score).sum()
}

pub fn part2(input: &str) -> u64 {
    let incomplete = input.lines().filter(|l| corrupted_score(l) == 0);

    let mut scores = vec![];

    for l in incomplete {
        let mut stack = vec![];

        for c in l.chars() {
            if "({[<".contains(c) {
                stack.push(c);
                continue;
            }

            if ")}]>".contains(c) {
                stack.pop().unwrap();
                continue;
            }

            unreachable!();
        }

        let mut score: u64 = 0;
        for c in stack.iter().rev() {
            let t = match c {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                '<' => 4,
                _ => unreachable!(),
            };
            score = score * 5 + t;
        }

        scores.push(score);
    }

    scores.sort();
    scores[scores.len() / 2]
}

fn corrupted_score(s: &str) -> u64 {
    let mut stack = vec![];
    for c in s.chars() {
        if "({[<".contains(c) {
            stack.push(c);
            continue;
        }

        if ")}]>".contains(c) {
            let opening = match stack.pop() {
                None => return 0, // unbalanced
                Some(oc) => oc,
            };

            if c == ')' {
                if opening != '(' {
                    return 3;
                }
                continue;
            }

            if c == ']' {
                if opening != '[' {
                    return 57;
                }
                continue;
            }

            if c == '}' {
                if opening != '{' {
                    return 1197;
                }
                continue;
            }

            if c == '>' {
                if opening != '<' {
                    return 25137;
                }
                continue;
            }
        }

        unreachable!("{}", c)
    }

    // unbalanced or correct
    0
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(339411, part1(include_str!("../input/day10.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(2289754624, part2(include_str!("../input/day10.txt")));
    }
}

pub fn part1(input: &str) -> usize {
    input
        .lines()
        .map(|l| {
            let (total, chars) = escaped_count(l);

            total - chars
        })
        .sum()
}

pub fn part2(input: &str) -> usize {
    input
        .lines()
        .map(|l| {
            let encoded = encode(l);

            let (original_total, _) = escaped_count(l);
            let (encoded_total, _) = escaped_count(&encoded);

            encoded_total - original_total
        })
        .sum()
}

fn escaped_count(s: &str) -> (usize, usize) {
    let mut total = 0;
    let mut chars = 0;

    let mut ci = s.chars();

    while let Some(c) = ci.next() {
        total += 1;
        chars += 1;

        if c == '\\' {
            let nc = ci.next().unwrap();
            total += 1;

            if nc == 'x' {
                ci.next().unwrap();
                ci.next().unwrap();

                total += 2;
            }
        }
    }

    (total, chars - 2)
}

fn encode(s: &str) -> String {
    format!(
        "\"{}\"",
        s.chars()
            .map(|c| match c {
                '\\' => "\\\\".to_string(),
                '"' => "\\\"".to_string(),
                c => c.to_string(),
            })
            .collect::<String>()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(1371, part1(include_str!("../input/day8.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(2117, part2(include_str!("../input/day8.txt")));
    }
}

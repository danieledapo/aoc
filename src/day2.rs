use std::collections::HashMap;

pub fn part1(input: &str) -> u64 {
    let mut c2 = 0;
    let mut c3 = 0;

    for l in input.lines() {
        let freqs = build_freq_hist(l);

        let (has_2, has_3) = freqs.into_iter().fold((false, false), |(e2, e3), (_, c)| {
            (e2 || c == 2, e3 || c == 3)
        });

        if has_2 {
            c2 += 1;
        }

        if has_3 {
            c3 += 1;
        }
    }

    c2 * c3
}

fn build_freq_hist(line: &str) -> HashMap<char, u64> {
    let mut freqs = HashMap::new();

    for c in line.chars() {
        *freqs.entry(c).or_default() += 1;
    }

    freqs
}

pub fn part2<'a>(input: &'a str) -> Option<String> {
    let zip_chars = |line1: &'a str, line2: &'a str| line1.chars().zip(line2.chars());

    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| input.lines().skip(i + 1).map(move |line2| (line, line2)))
        .find(|(line, line2)| zip_chars(line, line2).filter(|(c1, c2)| c1 != c2).count() == 1)
        .map(|(line, line2)| {
            zip_chars(line, line2)
                .filter(|(c1, c2)| c1 == c2)
                .map(|(c, _)| c)
                .collect::<String>()
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(5880, part1(include_str!("../input/day2.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(
            Some("tiwcdpbseqhxryfmgkvjujvza".to_string()),
            part2(include_str!("../input/day2.txt"))
        );
    }

}

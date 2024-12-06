use once_cell::sync::OnceCell;
use regex::Regex;

pub fn part1(input: &str) -> usize {
    parse(input)
        .filter(|&(min, max, c, pwd)| {
            let count = pwd.chars().filter(|cc| c == *cc).count();

            (min..=max).contains(&count)
        })
        .count()
}

pub fn part2(input: &str) -> usize {
    parse(input)
        .filter(|(i0, i1, c, pwd)| {
            let i0_valid = pwd.chars().nth(i0 - 1).unwrap() == *c;
            let i1_valid = pwd.chars().nth(i1 - 1).unwrap() == *c;

            i0_valid ^ i1_valid
        })
        .count()
}

fn parse<'s>(input: &'s str) -> impl Iterator<Item = (usize, usize, char, &str)> + 's {
    static RE: OnceCell<Regex> = OnceCell::new();
    RE.get_or_init(|| Regex::new(r"^(\d+)\-(\d+)\s*(.)\s*:\s*(.+)$").unwrap());

    input.lines().map(|l| {
        let re = RE.get().unwrap();
        let caps = re.captures(l).unwrap();

        let i0 = caps[1].parse::<usize>().unwrap();
        let i1 = caps[2].parse::<usize>().unwrap();
        let c = caps[3].chars().next().unwrap();
        let pwd = caps.get(4).unwrap().as_str();

        (i0, i1, c, pwd)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(517, part1(include_str!("../input/day2.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(284, part2(include_str!("../input/day2.txt")));
    }
}

use std::convert::TryFrom;

use crate::day5;

pub fn part1(input: &str) -> i64 {
    run(
        input,
        r#"
NOT A J
NOT J J
AND B J
AND C J
NOT J J
AND D J
WALK
"#,
    )
}

pub fn part2(input: &str) -> i64 {
    run(
        input,
        r#"
NOT A J
AND D J
NOT B T
AND D T
OR T J
NOT C T
AND D T
AND H T
OR T J
RUN
"#,
    )
}

fn run(input: &str, rules: &str) -> i64 {
    let mut prog = day5::Machine::new(input);

    let mut input = rules
        .trim_start()
        .as_bytes()
        .iter()
        .map(|c| i64::from(*c))
        .collect();

    let output = prog.run(&mut input);

    for c in &output {
        if let Ok(b) = u8::try_from(*c) {
            print!("{}", b as char);
        }
    }

    *output.last().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day21.txt")), 19359752);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day21.txt")), 1141869516);
    }
}

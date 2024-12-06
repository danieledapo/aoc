pub fn part1(input: &str) -> String {
    let keypad = [
        &['1', '2', '3'][..],
        &['4', '5', '6'][..],
        &['7', '8', '9'][..],
    ];
    run(&keypad, (1, 1), input)
}

pub fn part2(input: &str) -> String {
    let keypad = [
        &[' ', ' ', '1', ' ', ' '][..],
        &[' ', '2', '3', '4', ' '][..],
        &['5', '6', '7', '8', '9'][..],
        &[' ', 'A', 'B', 'C', ' '][..],
        &[' ', ' ', 'D', ' ', ' '][..],
    ];

    run(&keypad, (0, 2), input)
}

fn run(keypad: &[&[char]], mut p: (usize, usize), input: &str) -> String {
    let h = keypad.len();
    let w = keypad[0].len();

    let mut res = String::new();

    for l in input.lines() {
        p = l.chars().fold(p, |(x, y), c| match c {
            'U' if y > 0 && keypad[y - 1][x] != ' ' => (x, y - 1),
            'D' if y + 1 < h && keypad[y + 1][x] != ' ' => (x, y + 1),
            'L' if x > 0 && keypad[y][x - 1] != ' ' => (x - 1, y),
            'R' if x + 1 < w && keypad[y][x + 1] != ' ' => (x + 1, y),
            _ => (x, y),
        });

        res.push(keypad[p.1][p.0]);
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day2.txt")), "73597");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day2.txt")), "A47DA");
    }
}

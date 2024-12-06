pub fn part1(input: &str) -> i64 {
    let mut strength = 0;

    let mut cycle = 0;
    let mut x = 1;

    for (cycles, dx) in parse(input) {
        for _ in 0..cycles {
            cycle += 1;

            if cycle >= 20 && (cycle - 20) % 40 == 0 {
                strength += cycle * x;
            }
        }

        x += dx;
    }

    strength
}

pub fn part2(input: &str) -> Vec<String> {
    let mut display = vec![String::new(); 6];

    let mut cycle: i64 = 0;
    let mut x: i64 = 1;

    for (cycles, dx) in parse(input) {
        for _ in 0..cycles {
            debug_assert!(cycle >= 0);
            let row = (cycle / 40) as usize;
            let col = cycle % 40;

            let c = if (x - 1..=x + 1).contains(&col) {
                '#'
            } else {
                '.'
            };

            display[row].push(c);

            cycle += 1;
        }

        x += dx;
    }

    println!("{}", display.join("\n"));

    display
}

fn parse(input: &str) -> impl Iterator<Item = (i64, i64)> + '_ {
    input.lines().map(|l| {
        let mut parts = l.split_whitespace();

        match parts.next().unwrap() {
            "noop" => (1, 0),
            "addx" => (2, parts.next().unwrap().parse().unwrap()),
            _ => unreachable!(),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(13680, part1(include_str!("../input/day10.txt"),));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(
            r#"
###..####..##..###..#..#.###..####.###..
#..#....#.#..#.#..#.#.#..#..#.#....#..#.
#..#...#..#....#..#.##...#..#.###..###..
###...#...#.##.###..#.#..###..#....#..#.
#....#....#..#.#....#.#..#....#....#..#.
#....####..###.#....#..#.#....####.###.."#
                .trim(),
            part2(include_str!("../input/day10.txt")).join("\n")
        );
    }
}

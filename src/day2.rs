pub fn part1(input: &str) -> u32 {
    input
        .trim()
        .lines()
        .map(|l| parse_dimensions(l).unwrap())
        .map(|(l, w, h)| {
            let surface = 2 * l * w + 2 * w * h + 2 * h * l;
            let extra = (l * w).min(w * h).min(h * l);

            surface + extra
        })
        .sum()
}

pub fn part2(input: &str) -> u32 {
    input
        .trim()
        .lines()
        .map(|l| parse_dimensions(l).unwrap())
        .map(|(l, w, h)| {
            let peri = 2 * (l + w).min(l + h).min(w + h);
            let vol = l * w * h;

            peri + vol
        })
        .sum()
}

fn parse_dimensions(line: &str) -> Option<(u32, u32, u32)> {
    let mut parts = line.split('x');
    let l = parts.next()?.parse().ok()?;
    let w = parts.next()?.parse().ok()?;
    let h = parts.next()?.parse().ok()?;

    Some((l, w, h))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(1_606_483, part1(include_str!("../input/day2.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(3_842_356, part2(include_str!("../input/day2.txt")));
    }
}

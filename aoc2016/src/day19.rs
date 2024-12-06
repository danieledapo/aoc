pub fn part1(elves: i32) -> f64 {
    // thanks numberphile :)

    let elves = f64::from(elves);
    1.0 + 2.0 * (elves - 2.0_f64.powf(elves.log2().floor()))
}

pub fn part2(elves: i32) -> i32 {
    let mut w = 1;

    for i in 1..elves {
        w = w % i + 1;
        if w > (i + 1) / 2 {
            w += 1;
        }
    }

    w
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(3017957), 1841611.0);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(3017957), 1423634);
    }
}

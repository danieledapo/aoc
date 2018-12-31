pub fn part1(row: usize, col: usize) -> u64 {
    let diag = (row - 1) + (col - 1);
    let i = col - 1 + (1..=diag).fold(1, |v, r| v + r) - 1;

    (0..i).fold(20_151_125, |v, _| v * 252_533 % 33_554_393)
}

pub fn part2() -> &'static str {
    "Merry Christmas!"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(20_151_125, part1(1, 1));
        assert_eq!(31_916_031, part1(2, 1));
        assert_eq!(18_749_137, part1(1, 2));
        assert_eq!(21_629_792, part1(2, 2));

        assert_eq!(19_980_801, part1(2947, 3029));
    }

    #[test]
    fn solution_part2() {
        assert_eq!("Merry Christmas!", part2());
    }
}

pub fn part1(input: &str) -> u64 {
    evolve(input, 80)
}

pub fn part2(input: &str) -> u64 {
    evolve(input, 256)
}

fn evolve(input: &str, gens: usize) -> u64 {
    let mut fishes = input.trim().split(',').fold([0; 9], |mut fishes, n| {
        let n: usize = n.parse().unwrap();
        fishes[n] += 1;
        fishes
    });

    for _ in 0..gens {
        let newborn = fishes[0];
        fishes.rotate_left(1);
        fishes[6] += newborn;
    }

    fishes.iter().sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(388739, part1(include_str!("../input/day6.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1741362314973, part2(include_str!("../input/day6.txt")));
    }
}

//! There might be clever ways that involve primes and divisors, but the naive
//! way is reasonably fast in release mode.

pub fn part1(presents: usize) -> usize {
    let mut houses = vec![0; presents];

    for e in 0..houses.len() {
        for i in (e..houses.len()).step_by(e + 1) {
            houses[i] += (e + 1) * 10;
        }

        if houses[e] >= presents {
            return e + 1;
        }
    }

    unreachable!()
}

pub fn part2(presents: usize) -> usize {
    let mut houses = vec![0; presents];

    for e in 0..houses.len() {
        for i in (e..houses.len()).step_by(e + 1).take(50) {
            houses[i] += (e + 1) * 11;
        }

        if houses[e] >= presents {
            return e + 1;
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(831_600, part1(36_000_000));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(884_520, part2(36_000_000));
    }
}

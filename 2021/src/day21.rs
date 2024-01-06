use std::collections::HashMap;

pub fn part1(mut p1: u32, mut p2: u32) -> u32 {
    let mut n = 0;
    let mut die = 1;

    let mut p1_score = 0;
    let mut p2_score = 0;

    loop {
        for _ in 0..3 {
            p1 += die;

            if die >= 100 {
                die = 1;
            } else {
                die += 1;
            }
        }

        p1 = p1 % 10;
        if p1 == 0 {
            p1 = 10;
        }

        p1_score += p1;

        n += 3;

        if p1_score >= 1000 {
            return p2_score * n;
        }

        std::mem::swap(&mut p1, &mut p2);
        std::mem::swap(&mut p1_score, &mut p2_score);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Universe {
    p1: u32,
    p2: u32,
    p1_score: u32,
    p2_score: u32,
}

pub fn part2(p1: u32, p2: u32) -> u64 {
    let (p0_wins, p1_wins) = dirac_game(
        Universe {
            p1,
            p2,
            p1_score: 0,
            p2_score: 0,
        },
        &mut HashMap::new(),
    );

    p0_wins.max(p1_wins)
}

fn dirac_game(universe: Universe, cache: &mut HashMap<Universe, (u64, u64)>) -> (u64, u64) {
    if universe.p2_score >= 21 {
        return (0, 1);
    }

    if let Some(wins) = cache.get(&universe) {
        return *wins;
    }

    let mut wins = (0, 0);

    for roll in [
        3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9,
    ] {
        let mut p = (universe.p1 + roll) % 10;
        if p == 0 {
            p = 10;
        }

        let new_wins = dirac_game(
            Universe {
                p1: universe.p2,
                p2: p,
                p1_score: universe.p2_score,
                p2_score: universe.p1_score + p,
            },
            cache,
        );

        wins.0 += new_wins.1;
        wins.1 += new_wins.0;
    }

    cache.insert(universe, wins);

    wins
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(900099, part1(10, 6));
    }

    #[test]
    fn test_part2() {
        assert_eq!(306719685234774, part2(10, 6));
    }
}

use std::collections::BTreeSet as HashSet;

pub fn part1(input: &str) -> usize {
    let asteroids = parse(input);

    asteroids
        .iter()
        .map(|candidate| asteroids_in_sight(*candidate, &asteroids).len())
        .max()
        .unwrap()
}

pub fn part2(input: &str, mut n: usize) -> i32 {
    let mut asteroids = parse(input);

    let base = *asteroids
        .iter()
        .max_by_key(|candidate| asteroids_in_sight(**candidate, &asteroids).len())
        .unwrap();

    asteroids.remove(&base);
    loop {
        if asteroids.is_empty() {
            panic!("too few asteroids");
        }

        let mut in_sight = asteroids_in_sight(base, &asteroids);
        in_sight.sort_by_key(|(x, y)| {
            use std::f64::consts::PI;

            let mut a = f64::atan2(f64::from(x - base.0), f64::from(base.1 - y));
            if a < 0.0 {
                a += 2.0 * PI;
            }

            (a * 1_000_000_000_000.0) as i64
        });

        for a in in_sight {
            n -= 1;
            if n == 0 {
                return a.0 * 100 + a.1;
            }

            asteroids.remove(&a);
        }
    }
}

fn asteroids_in_sight((x, y): (i32, i32), asteroids: &HashSet<(i32, i32)>) -> Vec<(i32, i32)> {
    let mut in_sight = vec![];
    for &(ax, ay) in asteroids {
        if (ax, ay) == (x, y) {
            continue;
        }

        let mut dx = ax - x;
        let mut dy = ay - y;

        if dx == 0 || dy == 0 {
            dx = dx.signum();
            dy = dy.signum();
        } else {
            let divisor = gcd(dx, dy);
            if divisor != 0 {
                dx /= divisor;
                dy /= divisor;
            }
        }

        let is_in_sight = {
            let mut cx = x;
            let mut cy = y;
            loop {
                cx += dx;
                cy += dy;

                if cx == ax && cy == ay {
                    break true;
                }

                if asteroids.contains(&(cx, cy)) {
                    break false;
                }
            }
        };

        if is_in_sight {
            in_sight.push((ax, ay));
        }
    }

    in_sight
}

fn parse(input: &str) -> HashSet<(i32, i32)> {
    input
        .lines()
        .zip(0..)
        .flat_map(|(l, y)| {
            l.chars()
                .zip(0..)
                .filter(|(c, _)| *c == '#')
                .map(move |(_, x)| (x, y))
        })
        .collect()
}

fn gcd(mut m: i32, mut n: i32) -> i32 {
    while m != 0 {
        let old_m = m;
        m = n % m;
        n = old_m;
    }
    n.abs()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day10.txt")), 303);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day10.txt"), 200), 408);
    }
}

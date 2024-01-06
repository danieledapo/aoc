use std::collections::HashSet;

pub fn part1(input: &str) -> u64 {
    let mut rocks = parse(input);

    let maxy = rocks.iter().map(|(_, y)| *y).max().unwrap();
    let mut sand_rest = 0;

    loop {
        let mut sx = 500;
        let mut sy = 0;

        loop {
            if sy > maxy {
                return sand_rest;
            }

            let dx = [0, -1, 1]
                .into_iter()
                .find(|dx| !rocks.contains(&(sx + dx, sy + 1)));

            match dx {
                Some(dx) => {
                    sx += dx;
                    sy += 1;
                }
                None => {
                    rocks.insert((sx, sy));
                    sand_rest += 1;
                    break;
                }
            }
        }
    }
}

pub fn part2(input: &str) -> u64 {
    let mut rocks = parse(input);

    let floor_y = rocks.iter().map(|(_, y)| *y).max().unwrap() + 2;
    let mut sand_rest = 0;

    loop {
        let mut sx = 500;
        let mut sy = 0;

        loop {
            if sy + 1 >= floor_y {
                rocks.insert((sx, sy));
                sand_rest += 1;
                break;
            }

            let dx = [0, -1, 1]
                .into_iter()
                .find(|dx| !rocks.contains(&(sx + dx, sy + 1)));

            match dx {
                Some(dx) => {
                    sx += dx;
                    sy += 1;
                }
                None => {
                    if sy == 0 {
                        return sand_rest + 1;
                    }

                    rocks.insert((sx, sy));
                    sand_rest += 1;
                    break;
                }
            }
        }
    }
}

fn parse(input: &str) -> HashSet<(i64, i64)> {
    let mut res = HashSet::new();
    for l in input.lines() {
        let points = l
            .split(" -> ")
            .map(|l| {
                let (x, y) = l.split_once(',').unwrap();
                (x.parse::<i64>().unwrap(), y.parse::<i64>().unwrap())
            })
            .collect::<Vec<_>>();

        for w in points.windows(2) {
            let s = w[0];
            let e = w[1];

            let (dx, dy) = (e.0 - s.0, e.1 - s.1);
            let mut p = s;
            while p != e {
                res.insert(p);
                p = (p.0 + dx.signum(), p.1 + dy.signum());
            }
            res.insert(p);
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(1003, part1(include_str!("../input/day14.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(25771, part2(include_str!("../input/day14.txt")));
    }
}

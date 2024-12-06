use std::collections::HashSet;

pub fn part1(input: &str) -> usize {
    let mut blacks = HashSet::new();

    for path in input.lines() {
        let (x, y) = follow_path(&path);

        if !blacks.insert((x, y)) {
            blacks.remove(&(x, y));
        }
    }

    blacks.len()
}

pub fn part2(input: &str) -> usize {
    let mut blacks = HashSet::new();

    for path in input.lines() {
        let (x, y) = follow_path(&path);

        if !blacks.insert((x, y)) {
            blacks.remove(&(x, y));
        }
    }

    for _ in 0..100 {
        let minx = blacks.iter().map(|(x, _)| *x).min().unwrap();
        let maxx = blacks.iter().map(|(x, _)| *x).max().unwrap();
        let miny = blacks.iter().map(|(_, y)| *y).min().unwrap();
        let maxy = blacks.iter().map(|(_, y)| *y).max().unwrap();

        let mut new_blacks = HashSet::new();

        for y in (miny - 1)..=(maxy + 1) {
            let dx = y.abs() % 2;
            for x in (minx - 1)..=(maxx + 1) {
                let black_neighbors = [
                    (x - dx, y + 1),
                    (x - dx + 1, y + 1),
                    (x - 1, y),
                    (x + 1, y),
                    (x - dx, y - 1),
                    (x - dx + 1, y - 1),
                ]
                .iter()
                .filter(|&&(xx, yy)| blacks.contains(&(xx, yy)))
                .count();

                let is_black = blacks.contains(&(x, y));
                if black_neighbors == 2 || (is_black && black_neighbors == 1) {
                    new_blacks.insert((x, y));
                }
            }
        }

        blacks = new_blacks;
    }

    blacks.len()
}

fn follow_path(input: &str) -> (i64, i64) {
    let mut x: i64 = 0;
    let mut y: i64 = 0;

    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        if c == 'e' {
            x += 1;
            continue;
        }

        if c == 'w' {
            x -= 1;
            continue;
        }

        if c == 's' {
            x = x - y.abs() % 2;
            y += 1;

            let cc = chars.next().unwrap();
            if cc == 'e' {
                x += 1;
            }
            continue;
        }

        if c == 'n' {
            x = x - y.abs() % 2;
            y -= 1;

            let cc = chars.next().unwrap();
            if cc == 'e' {
                x += 1;
            }
            continue;
        }

        panic!()
    }

    (x, y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(459, part1(include_str!("../input/day24.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(4150, part2(include_str!("../input/day24.txt")));
    }
}

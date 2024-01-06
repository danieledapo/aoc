use std::collections::HashMap;

const ROCKS: &[&[(usize, usize)]] = &[
    &[(0, 0), (1, 0), (2, 0), (3, 0)],
    &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    &[(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
    &[(0, 0), (0, 1), (0, 2), (0, 3)],
    &[(0, 0), (1, 0), (0, 1), (1, 1)],
];

pub fn part1(input: &str) -> usize {
    go(input, 2022)
}

pub fn part2(input: &str) -> usize {
    go(input, 1000000000000)
}

fn go(input: &str, n: usize) -> usize {
    let rocks_dims: Vec<_> = ROCKS
        .iter()
        .map(|r| {
            (
                r.iter().map(|(x, _)| *x).max().unwrap(),
                r.iter().map(|(_, y)| *y).max().unwrap(),
            )
        })
        .collect();

    let mut maxy = 0;
    let mut rows = vec![];

    let mut chars = input.trim().chars().cycle();

    let mut seen = HashMap::new();
    let mut prev = 0;

    let mut i = 0;
    while i < n {
        let rocki = i % ROCKS.len();
        let rock = &ROCKS[rocki];

        let mut dx = 2;
        let mut dy = maxy + 3 + rocks_dims[rocki].1;

        rows.resize(dy + 1, 0);

        loop {
            let left = chars.next().unwrap() == '<';
            if left {
                if dx > 0
                    && rock
                        .iter()
                        .all(|(xx, yy)| (rows[dy - yy] & (1 << (xx + dx - 1))) == 0)
                {
                    dx -= 1;
                }
            } else if dx + rocks_dims[rocki].0 + 1 < 7
                && rock
                    .iter()
                    .all(|(xx, yy)| (rows[dy - yy] & (1 << (xx + dx + 1))) == 0)
            {
                dx += 1;
            }

            if rock
                .iter()
                .all(|(xx, yy)| dy - yy > 0 && (rows[dy - yy - 1] & (1 << (xx + dx))) == 0)
            {
                dy -= 1;
                continue;
            }

            for (xx, yy) in rock.iter() {
                rows[dy - yy] |= 1 << (xx + dx);
            }

            maxy = maxy.max(dy + 1);
            break;
        }

        if rows.len() >= 2000 {
            let k: Vec<_> = rows[rows.len() - 2000..].to_vec();
            if let Some((prev_maxy, j)) = seen.insert(k, (maxy, i)) {
                let amt = (n - i) / (i - j);
                prev += amt * (maxy - prev_maxy);
                i += amt * (i - j);
            }
        }

        i += 1;
    }

    prev + maxy
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(3109, part1(include_str!("../input/day17.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(1541449275365, part2(include_str!("../input/day17.txt")));
    }
}

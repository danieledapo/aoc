use std::collections::HashSet;

type HeightMap = Vec<Vec<u32>>;

pub fn part1(input: &str) -> u32 {
    let heightmap = parse(input);

    low_points(&heightmap)
        .into_iter()
        .map(|(r, c)| heightmap[r][c] + 1)
        .sum()
}

pub fn part2(input: &str) -> usize {
    let heightmap = parse(input);

    let lp = low_points(&heightmap);

    let mut basin_sizes = vec![];

    for p in lp {
        let mut seen = HashSet::new();
        let mut to_visit = vec![p];

        while let Some((r, c)) = to_visit.pop() {
            let value = heightmap[r][c];
            if value == 9 {
                continue;
            }

            if !seen.insert((r, c)) {
                continue;
            }

            if r > 0 && heightmap[r - 1][c] > value {
                to_visit.push((r - 1, c));
            }

            if r < heightmap.len() - 1 && heightmap[r + 1][c] > value {
                to_visit.push((r + 1, c));
            }

            if c > 0 && heightmap[r][c - 1] > value {
                to_visit.push((r, c - 1));
            }

            if c < heightmap[r].len() - 1 && heightmap[r][c + 1] > value {
                to_visit.push((r, c + 1));
            }
        }

        basin_sizes.push(seen.len());
    }

    basin_sizes.sort_by_key(|l| std::cmp::Reverse(*l));
    basin_sizes[0] * basin_sizes[1] * basin_sizes[2]
}

fn low_points(heightmap: &HeightMap) -> Vec<(usize, usize)> {
    let mut lp = vec![];

    for (r, row) in heightmap.iter().enumerate() {
        for (c, value) in row.iter().enumerate() {
            if r > 0 && heightmap[r - 1][c] <= *value {
                continue;
            }

            if r < heightmap.len() - 1 && heightmap[r + 1][c] <= *value {
                continue;
            }

            if c > 0 && heightmap[r][c - 1] <= *value {
                continue;
            }

            if c < row.len() - 1 && heightmap[r][c + 1] <= *value {
                continue;
            }

            lp.push((r, c));
        }
    }

    lp
}

fn parse(input: &str) -> HeightMap {
    input
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(528, part1(include_str!("../input/day9.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(920448, part2(include_str!("../input/day9.txt")));
    }
}

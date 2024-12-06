use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> usize {
    let mut elves = parse(input.trim());

    for i in 0..10 {
        evolve(&mut elves, i);
    }

    let minx = elves.iter().map(|(x, _)| *x).min().unwrap();
    let maxx = elves.iter().map(|(x, _)| *x).max().unwrap();
    let miny = elves.iter().map(|(_, y)| *y).min().unwrap();
    let maxy = elves.iter().map(|(_, y)| *y).max().unwrap();

    ((maxx - minx + 1) * (maxy - miny + 1)) as usize - elves.len()
}

pub fn part2(input: &str) -> usize {
    let mut elves = parse(input.trim());

    for i in 0.. {
        let prev = elves.clone();
        evolve(&mut elves, i);
        if prev == elves {
            return i + 1;
        }
    }

    unreachable!()
}

fn evolve(elves: &mut HashSet<(i64, i64)>, t: usize) {
    const DIRS: [[(i64, i64); 3]; 4] = [
        [(-1, -1), (0, -1), (1, -1)],
        [(-1, 1), (0, 1), (1, 1)],
        [(-1, -1), (-1, 0), (-1, 1)],
        [(1, -1), (1, 0), (1, 1)],
    ];

    let mut candidates: HashMap<(i64, i64), Vec<(i64, i64)>> = HashMap::new();

    for &(x, y) in elves.iter() {
        let has_neighbor = (-1..=1)
            .flat_map(|dx| (-1..=1).map(move |dy| (x + dx, y + dy)))
            .any(|k| k != (x, y) && elves.contains(&k));

        if !has_neighbor {
            continue;
        }

        for j in 0..4 {
            let d = DIRS[(t + j) % 4];
            let free = d.iter().all(|(dx, dy)| !elves.contains(&(x + dx, y + dy)));

            if free {
                candidates
                    .entry((x + d[1].0, y + d[1].1))
                    .or_default()
                    .push((x, y));

                break;
            }
        }
    }

    for (dst, srcs) in candidates {
        if srcs.len() != 1 {
            continue;
        }

        elves.remove(&srcs[0]);
        elves.insert(dst);
    }
}

fn parse(input: &str) -> HashSet<(i64, i64)> {
    input
        .lines()
        .zip(0..)
        .flat_map(|(l, row)| {
            l.chars()
                .zip(0..)
                .filter_map(move |(c, col)| (c == '#').then_some((col, row)))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(4000, part1(include_str!("../input/day23.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(1040, part2(include_str!("../input/day23.txt")));
    }
}

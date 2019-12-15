use std::collections::HashMap;

use crate::day5;

type Point = (i64, i64);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Cell {
    Free,
    Wall,
    Oxygen,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Direction {
    North = 1,
    South = 2,
    West = 3,
    East = 4,
}

const DIRS: [Direction; 4] = [
    Direction::North,
    Direction::South,
    Direction::West,
    Direction::East,
];

pub fn part1(input: &str) -> usize {
    let map = get_map(input);

    let oxygen_pos = map
        .iter()
        .find(|(_, c)| **c == Cell::Oxygen)
        .map(|(p, _)| *p)
        .unwrap();

    find_path_len(&map, (0, 0), oxygen_pos).unwrap()
}

pub fn part2(input: &str) -> usize {
    let map = get_map(input);

    flood_fill_oxygen(map)
}

fn find_path_len(map: &HashMap<Point, Cell>, start: Point, dst: Point) -> Option<usize> {
    use std::collections::hash_map::Entry;

    let mut distances = HashMap::new();
    distances.insert(dst, 0);

    let mut stack = vec![(dst, 0)];

    while let Some((p, d)) = stack.pop() {
        for pp in DIRS.iter().map(|d| d.apply_delta(p)) {
            if map[&pp] == Cell::Wall {
                continue;
            }

            let d = d + 1;
            let changed = match distances.entry(pp) {
                Entry::Vacant(v) => {
                    v.insert(d);
                    true
                }
                Entry::Occupied(mut o) => {
                    if d < *o.get() {
                        o.insert(d);
                        true
                    } else {
                        false
                    }
                }
            };

            if changed {
                stack.push((pp, d));
            }
        }
    }

    distances.get(&start).copied()
}

fn flood_fill_oxygen(mut map: HashMap<Point, Cell>) -> usize {
    let mut oxygen = map
        .iter()
        .filter(|(_, c)| **c == Cell::Oxygen)
        .map(|(p, _)| *p)
        .collect::<Vec<_>>();

    for i in 0.. {
        let mut new_oxygen = vec![];

        for p in oxygen.drain(..) {
            for pp in DIRS.iter().map(|d| d.apply_delta(p)) {
                match map.get_mut(&pp) {
                    Some(c) if *c == Cell::Free => {
                        *c = Cell::Oxygen;
                        new_oxygen.push(pp);
                    }
                    _ => {}
                }
            }
        }

        std::mem::swap(&mut new_oxygen, &mut oxygen);
        if oxygen.is_empty() {
            return i;
        }
    }

    unreachable!()
}

fn get_map(input: &str) -> HashMap<Point, Cell> {
    let mut prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldOutput);

    let mut cells = HashMap::new();
    let mut directions: Vec<Direction> = vec![];
    let mut cur_pos: Point = (0, 0);

    cells.insert(cur_pos, Cell::Free);

    loop {
        let (dir, newp) = match DIRS
            .iter()
            .map(|dir| (*dir, dir.apply_delta(cur_pos)))
            .find(|(_, p)| !cells.contains_key(p))
        {
            Some(d) => d,
            None => match directions.pop() {
                None => break,
                Some(dir) => {
                    let opposite_dir = dir.opposite();
                    let output = prog.run(&mut vec![opposite_dir as i64]);

                    assert_ne!(output, [0]);
                    cur_pos = opposite_dir.apply_delta(cur_pos);
                    continue;
                }
            },
        };

        let output = prog.run(&mut vec![dir as i64]);
        let c = match output[..] {
            [0] => Cell::Wall,
            [1] => Cell::Free,
            [2] => Cell::Oxygen,
            _ => unreachable!("{:?}", output),
        };

        cells.insert(newp, c);
        if c != Cell::Wall {
            directions.push(dir);
            cur_pos = newp;
        }
    }

    cells
}

impl Direction {
    fn apply_delta(self, pt: Point) -> (i64, i64) {
        let (dx, dy) = match self {
            Direction::North => (0, -1),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
            Direction::East => (1, 0),
        };

        (pt.0 + dx, pt.1 + dy)
    }
    fn opposite(self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
            Direction::East => Direction::West,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day15.txt")), 246);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day15.txt")), 376);
    }
}

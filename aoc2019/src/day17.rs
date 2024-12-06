use std::collections::HashSet;

use crate::day5;

type Point = (isize, isize);
type MoveCommand = (char, usize);

struct Map {
    scaffolds: HashSet<Point>,
    robot_pos: Point,

    width: isize,
    height: isize,
}

pub fn part1(input: &str) -> isize {
    let map = Map::new(day5::Machine::new(input));

    map.intersections().map(|(x, y)| x * y).sum()
}

pub fn part2(input: &str) -> i64 {
    let mut prog = day5::Machine::new(input);
    let map = Map::new(prog.clone());
    map.dump();

    let path = map.path();
    println!(
        "{}",
        path.iter()
            .map(|(d, c)| format!("{},{},", d, c))
            .collect::<String>()
            .trim_end_matches(',')
    );

    // find the patterns manually, it's simpler... *meh*
    // let path = "L12,L12,L6,L6,R8,R4,L12,L12,L12,L6,L6,L12,L6,R12,R8,R8,R4,L12,L12,L12,L6,L6,L12,L6,R12,R8,R8,R4,L12,L12,L12,L6,L6,L12,L6,R12,R8";
    let seq = b"A,B,A,C,B,A,C,B,A,C\n";
    let a = b"L,12,L,12,L,6,L,6\n";
    let b = b"R,8,R,4,L,12\n";
    let c = b"L,12,L,6,R,12,R,8\n";
    let video_feed = b"n\n";

    let mut input = seq
        .iter()
        .chain(a)
        .chain(b)
        .chain(c)
        .chain(video_feed)
        .map(|b| i64::from(*b))
        .collect();

    prog.prog[0] = 2;
    let outputs = prog.run(&mut input);
    *outputs.last().unwrap()
}

impl Map {
    fn new(mut prog: day5::Machine) -> Self {
        let outputs = prog.run(&mut vec![]);

        let mut x = 0;
        let mut y = 0;
        let mut robot_pos = (0, 0);
        let mut scaffolds = HashSet::new();

        let mut width = 0;

        for c in outputs {
            match c {
                10 => {
                    y += 1;
                    x = 0;
                    continue;
                }
                35 => {
                    scaffolds.insert((x, y));
                }
                46 => { /* empty */ }
                94 => {
                    robot_pos = (x, y);
                }
                n => unreachable!("{}", n),
            }

            x += 1;
            width = width.max(x);
        }

        Self {
            scaffolds,
            robot_pos,
            height: y.saturating_sub(1),
            width,
        }
    }

    fn intersections(&self) -> impl Iterator<Item = Point> + '_ {
        const DIRS: [Point; 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

        self.scaffolds
            .iter()
            .filter(move |(x, y)| {
                DIRS.iter()
                    .all(|(dx, dy)| self.scaffolds.contains(&(x + dx, y + dy)))
            })
            .copied()
    }

    fn path(&self) -> Vec<MoveCommand> {
        let mut path = vec![];

        let mut to_visit = self.scaffolds.clone();
        let mut pos = self.robot_pos;
        let mut dir = (0, -1);

        to_visit.remove(&pos);

        while !to_visit.is_empty() {
            let left = (dir.1, -dir.0);
            let right = (-dir.1, dir.0);

            let left_cell = to_visit.contains(&(pos.0 + left.0, pos.1 + left.1));
            let right_cell = to_visit.contains(&(pos.0 + right.0, pos.1 + right.1));

            dir = match (left_cell, right_cell) {
                (true, false) => left,
                (false, true) => right,
                _ => unreachable!("{:#?} {:?} {:?} {:?}", to_visit, pos, left_cell, right_cell),
            };

            let mut c = 0;
            loop {
                let p = ((pos.0 + dir.0), (pos.1 + dir.1));
                if !self.scaffolds.contains(&p) {
                    break;
                }

                c += 1;
                to_visit.remove(&p);
                pos = p;
            }

            path.push(if dir == left { ('L', c) } else { ('R', c) });
        }

        path
    }

    fn dump(&self) {
        for y in 0..self.height {
            let l = (0..self.width)
                .map(|x| {
                    if self.robot_pos == (x, y) {
                        return '^';
                    }

                    if self.scaffolds.contains(&(x, y)) {
                        return '#';
                    }

                    '.'
                })
                .collect::<String>();

            println!("{}", l);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day17.txt")), 4800);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day17.txt")), 982279);
    }
}

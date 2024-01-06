use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

pub type Pos = (usize, usize);

pub fn part1(input: &str) -> u64 {
    let (start, end, _, heightmap) = parse(input);
    go(&[start], end, &heightmap).unwrap()
}

pub fn part2(input: &str) -> u64 {
    let (start, end, mut aa, heightmap) = parse(input);
    // I guess it's not strictly needed as hopefully a starting position in aa
    // is quicker (otherwise it wouldn't make sense, but eh)
    aa.push(start);
    go(&aa, end, &heightmap).unwrap()
}

fn go(start: &[Pos], end: Pos, heightmap: &Vec<Vec<u32>>) -> Option<u64> {
    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::new();
    for start in start {
        queue.push(Reverse((0, *start)));
    }

    while let Some(Reverse((c, pos))) = queue.pop() {
        if !visited.insert(pos) {
            continue;
        }

        if pos == end {
            return Some(c);
        }

        let mut check = |x: usize, y: usize| {
            if heightmap[y][x] <= heightmap[pos.1][pos.0]
                || heightmap[y][x] == heightmap[pos.1][pos.0] + 1
            {
                queue.push(Reverse((c + 1, (x, y))));
            }
        };

        if pos.0 > 0 {
            check(pos.0 - 1, pos.1);
        }

        if pos.1 > 0 {
            check(pos.0, pos.1 - 1);
        }

        if pos.0 < heightmap[pos.1].len() - 1 {
            check(pos.0 + 1, pos.1);
        }

        if pos.1 < heightmap.len() - 1 {
            check(pos.0, pos.1 + 1);
        }
    }

    None
}

fn parse(input: &str) -> (Pos, Pos, Vec<Pos>, Vec<Vec<u32>>) {
    let mut res = vec![];
    let mut start = (0, 0);
    let mut end = (0, 0);
    let mut aa = vec![];

    for (row, l) in input.lines().enumerate() {
        let mut r = vec![];
        for (col, mut c) in l.chars().enumerate() {
            if c == 'S' {
                start = (col, row);
                c = 'a';
            } else if c == 'E' {
                end = (col, row);
                c = 'z';
            } else if c == 'a' {
                aa.push((col, row));
            }

            r.push(c as u32);
        }
        res.push(r);
    }

    (start, end, aa, res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(423, part1(include_str!("../input/day12.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(416, part2(include_str!("../input/day12.txt")));
    }
}

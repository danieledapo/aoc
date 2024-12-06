use std::collections::{BTreeMap, BTreeSet, BinaryHeap};

type Point = (i64, i64);

#[derive(Debug, Clone)]
struct Map {
    walls: BTreeSet<Point>,
    targets: BTreeMap<Point, u32>,
}

pub fn part1(input: &str) -> i64 {
    let map = parse(input);

    let distances = map.targets_distances();
    let pos0 = *map.targets.iter().find(|(_, id)| **id == 0).unwrap().0;

    let mut seen = BTreeSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0, BTreeSet::new(), pos0));

    while let Some((d, visited, pos)) = queue.pop() {
        let d = -d;
        if visited.len() == distances.len() {
            return d;
        }

        if !seen.insert((pos, visited.clone())) {
            continue;
        }

        for (np, s) in &distances[&pos] {
            if visited.contains(np) {
                continue;
            }

            let mut visited = visited.clone();
            visited.insert(*np);

            let key = (*np, visited);
            if seen.contains(&key) {
                continue;
            }

            queue.push((-(d + s), key.1, key.0));
        }
    }

    unreachable!()
}

pub fn part2(input: &str) -> i64 {
    let map = parse(input);

    let distances = map.targets_distances();
    let pos0 = *map.targets.iter().find(|(_, id)| **id == 0).unwrap().0;

    let mut seen = BTreeSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0, BTreeSet::new(), pos0));

    while let Some((d, visited, pos)) = queue.pop() {
        let d = -d;
        if visited.len() == distances.len() {
            if pos == pos0 {
                return d;
            }

            let s = distances[&pos][&pos0];
            queue.push((-(d + s), visited, pos0));

            continue;
        }

        if !seen.insert((pos, visited.clone())) {
            continue;
        }

        for (np, s) in &distances[&pos] {
            if visited.contains(np) {
                continue;
            }

            let mut visited = visited.clone();
            visited.insert(*np);

            let key = (*np, visited);
            if seen.contains(&key) {
                continue;
            }

            queue.push((-(d + s), key.1, key.0));
        }
    }

    unreachable!()
}

impl Map {
    fn targets_distances(&self) -> BTreeMap<Point, BTreeMap<Point, i64>> {
        let mut distances: BTreeMap<_, _> = self
            .targets
            .iter()
            .map(|(p, _)| (*p, BTreeMap::new()))
            .collect();

        for src in self.targets.keys() {
            for dst in self.targets.keys() {
                if distances[src].contains_key(dst) {
                    continue;
                }

                let d = self.shortest_path(*src, *dst);
                distances.get_mut(src).unwrap().insert(*dst, d);
                distances.get_mut(dst).unwrap().insert(*src, d);
            }
        }

        distances
    }

    fn shortest_path(&self, src: Point, dst: Point) -> i64 {
        let mut queue = BinaryHeap::new();
        queue.push((0, src));

        let mut seen = BTreeSet::new();
        while let Some((d, p)) = queue.pop() {
            let d = -d;

            if p == dst {
                return d;
            }

            if !seen.insert(p) {
                continue;
            }

            for &(x, y) in &[
                (p.0, p.1 - 1),
                (p.0, p.1 + 1),
                (p.0 - 1, p.1),
                (p.0 + 1, p.1),
            ] {
                if seen.contains(&(x, y)) {
                    continue;
                }

                if self.walls.contains(&(x, y)) {
                    continue;
                }

                queue.push((-(d + 1), (x, y)));
            }
        }

        unreachable!()
    }
}

fn parse(input: &str) -> Map {
    let mut map = Map {
        walls: BTreeSet::new(),
        targets: BTreeMap::new(),
    };

    for (l, y) in input.lines().zip(0..) {
        for (c, x) in l.chars().zip(0..) {
            match c {
                '.' => continue,
                '#' => {
                    map.walls.insert((x, y));
                }
                c => {
                    if let Some(d) = c.to_digit(10) {
                        map.targets.insert((x, y), d);
                    } else {
                        unreachable!()
                    }
                }
            }
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day24.txt")), 428);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day24.txt")), 680);
    }
}

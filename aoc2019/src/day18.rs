use std::collections::{BTreeSet, BinaryHeap, HashMap, HashSet};

type Point = (i64, i64);
type Grid = HashMap<Point, Cell>;
type Graph = HashMap<Point, Vec<(Point, i64)>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Cell {
    Key(char),
    Door(char),
    Wall,
    Entrance,
}

pub fn part1(input: &str) -> i64 {
    let grid = parse(input);
    search(&grid).unwrap()
}

pub fn part2(input: &str) -> i64 {
    let mut grid = parse(input);

    let (x, y) = grid
        .iter()
        .filter_map(|(p, c)| {
            if let Cell::Entrance = c {
                Some(*p)
            } else {
                None
            }
        })
        .next()
        .unwrap();

    for dy in -1..=1 {
        for dx in -1..=1 {
            grid.insert(
                (x + dx, y + dy),
                if dx != 0 && dy != 0 {
                    Cell::Entrance
                } else {
                    Cell::Wall
                },
            );
        }
    }
    search(&grid).unwrap()
}

fn search(grid: &Grid) -> Option<i64> {
    let graph = make_graph(grid);
    let nkeys = grid
        .values()
        .filter(|c| if let Cell::Key(_) = c { true } else { false })
        .count();
    let positions = grid
        .iter()
        .filter_map(|(p, c)| {
            if let Cell::Entrance = c {
                Some(*p)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0, positions, BTreeSet::new()));

    while let Some((d, positions, keys)) = queue.pop() {
        let d = -d;

        if keys.len() == nkeys {
            return Some(d);
        }

        let key = (positions.clone(), keys.clone());
        if !visited.insert(key) {
            continue;
        }

        for (i, pos) in positions.iter().enumerate() {
            for (neighbor, cost) in &graph[pos] {
                let gc = grid.get(&neighbor);
                if let Some(Cell::Door(dc)) = gc {
                    if !keys.contains(&dc.to_ascii_lowercase()) {
                        continue;
                    }
                }

                let mut new_pos = Vec::with_capacity(positions.len());
                new_pos.extend_from_slice(&positions[..i]);
                new_pos.push(*neighbor);
                new_pos.extend_from_slice(&positions[i + 1..]);

                let mut new_keys = keys.clone();
                if let Some(Cell::Key(kc)) = gc {
                    new_keys.insert(kc);
                }

                let k = (new_pos, new_keys);
                if visited.contains(&k) {
                    continue;
                }

                queue.push((-(d + cost), k.0, k.1));
            }
        }
    }

    None
}

fn make_graph(grid: &Grid) -> Graph {
    let mut graph: Graph = HashMap::new();
    let waypoints = grid
        .iter()
        .filter(|(_, c)| match c {
            Cell::Wall => false,
            _ => true,
        })
        .map(|(k, _)| *k)
        .collect::<Vec<_>>();

    for &a in &waypoints {
        for &b in &waypoints {
            if a <= b {
                continue;
            }

            if let Some(d) = shortest_path(grid, a, b) {
                graph.entry(a).or_default().push((b, d));
                graph.entry(b).or_default().push((a, d));
            }
        }
    }

    graph
}

fn shortest_path(grid: &Grid, src: Point, target: Point) -> Option<i64> {
    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0, src));

    while let Some((d, position)) = queue.pop() {
        let d = -d;

        if position == target {
            return Some(d);
        }

        if !visited.insert(position) {
            continue;
        }

        for &(dx, dy) in &[(0, -1), (0, 1), (-1, 0), (1, 0)] {
            let new_pos = (position.0 + dx, position.1 + dy);
            if visited.contains(&new_pos) || new_pos != target && grid.contains_key(&new_pos) {
                continue;
            }

            queue.push((-(d + 1), new_pos));
        }
    }

    None
}

fn parse(input: &str) -> Grid {
    let mut grid = HashMap::new();

    let mut y = 0;
    for l in input.lines() {
        let mut x = 0;

        for c in l.chars() {
            let cell = match c {
                '.' => {
                    x += 1;
                    continue;
                }
                '#' => Cell::Wall,
                '@' => Cell::Entrance,
                c if c.is_ascii_lowercase() => Cell::Key(c),
                c => Cell::Door(c),
            };

            grid.insert((x, y), cell);

            x += 1;
        }

        y += 1;
    }

    grid
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day18.txt")), 3586);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day18.txt")), 1974);
    }
}

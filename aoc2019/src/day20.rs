use std::collections::{BinaryHeap, HashMap, HashSet};

type Point = (i64, i64);
type Portal = [char; 2];

#[derive(Debug, Clone, PartialEq, Eq)]
struct Map {
    walls: HashSet<Point>,
    portals: HashMap<Portal, Vec<Point>>,
    width: i64,
    height: i64,
}

pub fn part1(input: &[u8]) -> i64 {
    let mut input = input.to_vec();
    let m = parse(&mut input);

    shortest_path(
        &m,
        m.portals[&['A', 'A']][0],
        m.portals[&['Z', 'Z']][0],
        false,
    )
    .unwrap()
}

pub fn part2(input: &[u8]) -> i64 {
    let mut input = input.to_vec();
    let m = parse(&mut input);

    shortest_path(
        &m,
        m.portals[&['A', 'A']][0],
        m.portals[&['Z', 'Z']][0],
        true,
    )
    .unwrap()
}

fn shortest_path(map: &Map, src: Point, dst: Point, use_levels: bool) -> Option<i64> {
    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0, src, 0));

    let mut portals = HashMap::new();
    for e in map.portals.values() {
        if e.len() != 2 {
            continue;
        }

        let out_ring =
            |(x, y): Point| x == 2 || y == 2 || x == map.width - 3 || y == map.height - 3;

        portals.insert(e[0], (e[1], !out_ring(e[0])));
        portals.insert(e[1], (e[0], !out_ring(e[1])));
    }

    while let Some((d, p, level)) = queue.pop() {
        let d = -d;
        if !visited.insert((p, level)) {
            continue;
        }

        if p == dst && level == 0 {
            return Some(d);
        }

        for &(dx, dy) in &[(0, -1), (0, 1), (-1, 0), (1, 0)] {
            let mut np = (p.0 + dx, p.1 + dy);

            if !(0..map.width).contains(&np.0) || !(0..map.height).contains(&np.1) {
                continue;
            }

            if map.walls.contains(&np) {
                continue;
            }

            let mut d = d;
            let mut level = level;
            if np != dst {
                if let Some((pp, inner)) = portals.get(&np) {
                    np = *pp;
                    d += 1;
                    level += if *inner { 1 } else { -1 };
                }
            }

            level = if !use_levels { 0 } else { level };
            if level < 0 {
                continue;
            }

            if visited.contains(&(np, level)) {
                continue;
            }

            queue.push((-(d + 1), np, level));
        }
    }

    None
}

fn parse(input: &mut [u8]) -> Map {
    let mut grid = input.split_mut(|b| *b == b'\n').collect::<Vec<_>>();
    if grid.last().unwrap().is_empty() {
        grid.pop();
    }

    let h = grid.len();

    let mut map = Map {
        walls: HashSet::new(),
        portals: HashMap::new(),
        width: 0,
        height: grid.len() as i64,
    };

    for y in 0..grid.len() {
        map.width = map.width.max(grid[y].len() as i64);

        for x in 0..grid[y].len() {
            match grid[y][x] {
                b'.' => {}
                b'#' | b' ' => {
                    map.walls.insert((x as i64, y as i64));
                }
                c => {
                    if x < grid[y + 1].len() && grid[y + 1][x] != b' ' {
                        let ey = if y == 0 || (y < h - 2 && grid[y + 2][x] == b'.') {
                            y + 2
                        } else {
                            y - 1
                        };
                        map.portals
                            .entry([c as char, grid[y + 1][x] as char])
                            .or_default()
                            .push((x as i64, ey as i64));
                        grid[y + 1][x] = b' ';

                        map.walls.insert((x as i64, y as i64));
                        map.walls.insert((x as i64, (y + 1) as i64));
                    } else if grid[y][x + 1] != b' ' {
                        let ex = if x == 0 || (x < grid[y].len() - 2 && grid[y][x + 2] == b'.') {
                            x + 2
                        } else {
                            x - 1
                        };
                        map.portals
                            .entry([c as char, grid[y][x + 1] as char])
                            .or_default()
                            .push((ex as i64, y as i64));
                        grid[y][x + 1] = b' ';

                        map.walls.insert((x as i64, y as i64));
                        map.walls.insert(((x + 1) as i64, y as i64));
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
        assert_eq!(part1(include_bytes!("../input/day20.txt")), 654);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_bytes!("../input/day20.txt")), 7360);
    }
}

use std::collections::HashMap;

type Point = (i64, i64);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Map {
    grid: Vec<Vec<Cell>>,
    doors: HashMap<char, Point>,
    entrance: Point,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Cell {
    Key(char),
    Door(char),
    Empty,
    Wall,
}

pub fn part1(input: &str) -> i64 {
    let map = Map::new(input);

    // for ((x, y), d) in map.reacheable_keys(map.entrance) {
    //     println!("{:?} {:?}", map.grid[y as usize][x as usize], d);
    // }

    min_steps(&map, map.entrance, 0, i64::max_value()).unwrap()
}

fn min_steps(map: &Map, p: Point, curd: i64, mut min_d: i64) -> Option<i64> {
    let mut reachable = map.reacheable_keys(p).into_iter().collect::<Vec<_>>();
    reachable.sort_by_key(|(_, d)| *d);

    if reachable.is_empty() {
        return if curd < min_d { Some(curd) } else { None };
    }

    reachable
        .into_iter()
        .filter_map(|((x, y), d)| {
            if curd + d >= min_d {
                return None;
            }

            let mut map = map.clone();

            let kc = match map.grid[y as usize][x as usize] {
                Cell::Key(k) => k,
                _ => unreachable!(),
            };

            map.grid[y as usize][x as usize] = Cell::Empty;

            if let Some(&(doorx, doory)) = map.doors.get(&kc) {
                map.grid[doory as usize][doorx as usize] = Cell::Empty;
            }

            let s = min_steps(&map, (x, y), curd + d, min_d)?;

            min_d = min_d.min(s);
            Some(s)
        })
        .min()
}

impl Map {
    fn new(input: &str) -> Self {
        let mut entrance = (0, 0);
        let mut grid = vec![];
        let mut doors = HashMap::new();

        let mut y = 0;
        for l in input.lines() {
            let mut x = 0;
            let mut line = vec![];
            for c in l.chars() {
                let cell = match c {
                    '.' => Cell::Empty,
                    '#' => Cell::Wall,
                    '@' => {
                        entrance = (x, y);
                        Cell::Empty
                    }
                    c if c.is_lowercase() => Cell::Key(c),
                    c => {
                        doors.insert(c.to_ascii_lowercase(), (x, y));
                        Cell::Door(c)
                    }
                };

                line.push(cell);

                x += 1;
            }

            grid.push(line);
            y += 1;
        }

        Self {
            grid,
            entrance,
            doors,
        }
    }

    fn reacheable_keys(&self, p: Point) -> HashMap<Point, i64> {
        const DIRS: [Point; 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

        let (w, h) = (self.grid[0].len(), self.grid.len());
        let mut distances = vec![vec![i64::max_value(); w]; h];

        let mut reached_keys = HashMap::new();

        let mut stack = vec![(p, 0)];
        while let Some(((x, y), d)) = stack.pop() {
            if x < 0 || x >= w as i64 || y < 0 || y >= h as i64 {
                continue;
            }

            if distances[y as usize][x as usize] <= d {
                continue;
            }
            distances[y as usize][x as usize] = distances[y as usize][x as usize].min(d);

            match self.grid[y as usize][x as usize] {
                Cell::Wall | Cell::Door(_) => continue,
                Cell::Key(_) => {
                    reached_keys.insert((x, y), 0);
                    continue;
                }
                Cell::Empty => {}
            }

            stack.extend(DIRS.iter().map(|(dx, dy)| ((x + dx, y + dy), d + 1)));
        }

        for (p, v) in &mut reached_keys {
            *v = distances[p.1 as usize][p.0 as usize];
        }

        reached_keys
    }

    fn iter_cells(&self) -> impl Iterator<Item = (Point, Cell)> + '_ {
        self.grid.iter().enumerate().flat_map(|(y, l)| {
            l.iter()
                .enumerate()
                .map(move |(x, c)| ((x as i64, y as i64), *c))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        unimplemented!();

        assert_eq!(
            part1(
                r#"#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"#
            ),
            136
        );
        panic!();

        assert_eq!(
            part1(
                r#"#########
#b.A.@.a#
#########"#
            ),
            8
        );
        assert_eq!(
            part1(
                r#"########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"#
            ),
            86
        );
        assert_eq!(
            part1(
                r#"########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"#
            ),
            132
        );
        assert_eq!(
            part1(
                r#"########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"#
            ),
            81
        );
        // assert_eq!(part1(include_str!("../input/day18.txt")), 10);
    }
}

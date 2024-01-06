use std::collections::{BTreeMap, HashSet};

const H: i16 = 5;
const W: i16 = 5;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct World(u32);

impl World {
    pub fn zero() -> Self {
        World(0)
    }

    pub fn parse(input: &str) -> Self {
        use std::convert::TryFrom;

        let w = input
            .lines()
            .enumerate()
            .flat_map(|(y, l)| {
                l.chars().enumerate().map(move |(x, c)| match c {
                    '#' => 1 << (y * usize::try_from(W).unwrap() + x),
                    '.' => 0,
                    _ => unreachable!(),
                })
            })
            .sum();

        World(w)
    }

    pub fn get(self, x: i16, y: i16) -> u32 {
        if x < 0 || y < 0 || x >= W || y >= H {
            0
        } else {
            (self.0 >> (y * H + x)) & 1
        }
    }

    pub fn set(&mut self, x: i16, y: i16, bit: u32) {
        assert!(x >= 0 && x < W);
        assert!(y >= 0 && y < H);

        self.0 |= bit << (y * H + x);
    }

    pub fn first_row_bugs(self) -> u32 {
        (0..W).map(|x| self.get(x, 0)).sum()
    }

    pub fn first_col_bugs(self) -> u32 {
        (0..H).map(|y| self.get(0, y)).sum()
    }

    pub fn last_row_bugs(self) -> u32 {
        (0..W).map(|x| self.get(x, H - 1)).sum()
    }

    pub fn last_col_bugs(self) -> u32 {
        (0..H).map(|y| self.get(W - 1, y)).sum()
    }

    pub fn bugs(self) -> u32 {
        self.0.count_ones()
    }
}

pub fn part1(input: &str) -> u32 {
    let mut world = World::parse(input);
    let mut seen = HashSet::new();

    while seen.insert(world) {
        let mut new_world = World::zero();

        for y in 0..H {
            for x in 0..W {
                let c = world.get(x, y);

                let alive = world.get(x, y - 1)
                    + world.get(x, y + 1)
                    + world.get(x - 1, y)
                    + world.get(x + 1, y);

                let newc = if c == 1 && alive != 1 {
                    0
                } else if c == 0 && (1..=2).contains(&alive) {
                    1
                } else {
                    c
                };

                new_world.set(x, y, newc);
            }
        }

        world = new_world;
    }

    world.0
}

pub fn part2(input: &str, minutes: usize) -> u32 {
    let mut worlds = BTreeMap::new();
    worlds.insert(0, World::parse(input));

    let gen_world = |worlds: &BTreeMap<i32, World>, depth, world: World| {
        let mut new_world = World::zero();

        for y in 0..H {
            for x in 0..W {
                // cannot change subworld
                if x == 2 && y == 2 {
                    continue;
                }

                let c = world.get(x, y);

                let mut alive = 0;

                // row below
                if y == 1 && x == 2 {
                    alive += worlds.get(&(depth + 1)).map_or(0, |w| w.first_row_bugs());
                } else if y == H - 1 {
                    alive += worlds.get(&(depth - 1)).map_or(0, |w| w.get(2, 3));
                } else {
                    alive += world.get(x, y + 1);
                }

                // row above
                if y == 3 && x == 2 {
                    alive += worlds.get(&(depth + 1)).map_or(0, |w| w.last_row_bugs());
                } else if y == 0 {
                    alive += worlds.get(&(depth - 1)).map_or(0, |w| w.get(2, 1));
                } else {
                    alive += world.get(x, y - 1);
                }

                // left column
                if y == 2 && x == 3 {
                    alive += worlds.get(&(depth + 1)).map_or(0, |w| w.last_col_bugs());
                } else if x == 0 {
                    alive += worlds.get(&(depth - 1)).map_or(0, |w| w.get(1, 2));
                } else {
                    alive += world.get(x - 1, y);
                }

                // right column
                if y == 2 && x == 1 {
                    alive += worlds.get(&(depth + 1)).map_or(0, |w| w.first_col_bugs());
                } else if x == W - 1 {
                    alive += worlds.get(&(depth - 1)).map_or(0, |w| w.get(3, 2));
                } else {
                    alive += world.get(x + 1, y);
                }

                let newc = if c == 1 && alive != 1 {
                    0
                } else if c == 0 && (1..=2).contains(&alive) {
                    1
                } else {
                    c
                };

                new_world.set(x, y, newc);
            }
        }

        new_world
    };

    for _ in 0..minutes {
        let mut new_worlds = BTreeMap::new();

        for (depth, world) in &worlds {
            let new_world = gen_world(&worlds, *depth, *world);
            new_worlds.insert(*depth, new_world);
        }

        let min_depth = *worlds.keys().next().unwrap();
        let max_depth = *worlds.keys().next_back().unwrap();

        let bigger_world = gen_world(&worlds, min_depth - 1, World::zero());
        if bigger_world.bugs() > 0 {
            new_worlds.insert(min_depth - 1, bigger_world);
        }

        let smaller_world = gen_world(&worlds, max_depth + 1, World::zero());
        if smaller_world.bugs() > 0 {
            new_worlds.insert(max_depth + 1, smaller_world);
        }

        worlds = new_worlds;
    }

    worlds.values().map(|b| b.bugs()).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day24.txt")), 18842609);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day24.txt"), 200), 2059);
    }
}

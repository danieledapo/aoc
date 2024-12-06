use std::collections::HashSet;

struct World {
    alive: HashSet<(i64, i64, i64, i64)>,
    tl: (i64, i64, i64, i64),
    br: (i64, i64, i64, i64),
}

pub fn part1(input: &str) -> usize {
    let mut world = input.parse::<World>().unwrap();

    for _ in 0..6 {
        let mut new_world = World::new();

        let zs = world.tl.2 - 1..=world.br.2 + 1;
        for z in zs {
            let ys = world.tl.1 - 1..=world.br.1 + 1;

            for y in ys {
                let xs = world.tl.0 - 1..=world.br.0 + 1;

                for x in xs {
                    let neighbors = world.alive_neighbors3(x, y, z, 0);
                    let alive = world.is_alive(x, y, z, 0);

                    if neighbors == 3 || (alive && neighbors == 2) {
                        new_world.set_alive(x, y, z, 0);
                    }
                }
            }
        }

        std::mem::swap(&mut world, &mut new_world);
    }

    world.alive.len()
}

pub fn part2(input: &str) -> usize {
    let mut world = input.parse::<World>().unwrap();

    for _ in 0..6 {
        let mut new_world = World::new();

        let ws = world.tl.3 - 1..=world.br.3 + 1;

        for w in ws {
            let zs = world.tl.2 - 1..=world.br.2 + 1;
            for z in zs {
                let ys = world.tl.1 - 1..=world.br.1 + 1;

                for y in ys {
                    let xs = world.tl.0 - 1..=world.br.0 + 1;

                    for x in xs {
                        let neighbors = world.alive_neighbors4(x, y, z, w);
                        let alive = world.is_alive(x, y, z, w);

                        if neighbors == 3 || (alive && neighbors == 2) {
                            new_world.set_alive(x, y, z, w);
                        }
                    }
                }
            }
        }

        std::mem::swap(&mut world, &mut new_world);
    }

    world.alive.len()
}

impl World {
    fn new() -> World {
        World {
            alive: HashSet::new(),
            tl: (
                i64::max_value(),
                i64::max_value(),
                i64::max_value(),
                i64::max_value(),
            ),
            br: (
                i64::min_value(),
                i64::min_value(),
                i64::min_value(),
                i64::min_value(),
            ),
        }
    }

    fn set_alive(&mut self, x: i64, y: i64, z: i64, w: i64) {
        self.alive.insert((x, y, z, w));

        self.tl = (
            self.tl.0.min(x),
            self.tl.1.min(y),
            self.tl.2.min(z),
            self.tl.3.min(w),
        );
        self.br = (
            self.br.0.max(x),
            self.br.1.max(y),
            self.br.2.max(z),
            self.br.3.max(w),
        );
    }

    fn is_alive(&self, x: i64, y: i64, z: i64, w: i64) -> bool {
        self.alive.contains(&(x, y, z, w))
    }

    fn alive_neighbors3(&self, x: i64, y: i64, z: i64, w: i64) -> usize {
        let mut c = 0;
        for dz in -1..=1 {
            let z = z + dz;

            for dy in -1..=1 {
                let y = y + dy;

                for dx in -1..=1 {
                    let x = x + dx;

                    if dz != 0 || dy != 0 || dx != 0 {
                        if self.alive.contains(&(x, y, z, w)) {
                            c += 1;
                        }
                    }
                }
            }
        }
        c
    }

    fn alive_neighbors4(&self, x: i64, y: i64, z: i64, w: i64) -> usize {
        let mut c = 0;

        for dw in -1..=1 {
            let w = w + dw;

            for dz in -1..=1 {
                let z = z + dz;

                for dy in -1..=1 {
                    let y = y + dy;

                    for dx in -1..=1 {
                        let x = x + dx;

                        if dz != 0 || dy != 0 || dx != 0 || dw != 0 {
                            if self.alive.contains(&(x, y, z, w)) {
                                c += 1;
                            }
                        }
                    }
                }
            }
        }
        c
    }
}

impl std::str::FromStr for World {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut w = World::new();

        let mut y = 0;
        for l in s.lines() {
            let mut x = 0;
            for c in l.chars() {
                if c == '#' {
                    w.set_alive(x, y, 0, 0);
                }
                x += 1;
            }
            y += 1;
        }

        Ok(w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(255, part1(include_str!("../input/day17.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(2340, part2(include_str!("../input/day17.txt")));
    }
}

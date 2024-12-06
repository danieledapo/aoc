use crate::util::IVec;

#[derive(Clone, PartialEq, Eq)]
pub struct World {
    seats: IVec<IVec<Seat>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Seat {
    Floor,
    Free,
    Occupied,
}

pub fn part1(input: &str) -> usize {
    let mut world = input.parse::<World>().unwrap();
    world.evolve_until_stall(4, 1);
    world.count_occupied()
}

pub fn part2(input: &str) -> usize {
    let mut world = input.parse::<World>().unwrap();
    world.evolve_until_stall(5, i32::max_value());
    world.count_occupied()
}

impl World {
    pub fn evolve_until_stall(&mut self, occupied_threshold: usize, max_r: i32) {
        let mut new_world = self.clone();

        loop {
            self.evolve(&mut new_world, occupied_threshold, max_r);
            std::mem::swap(self, &mut new_world);
            if self == &new_world {
                break;
            }
        }
    }

    pub fn evolve(&self, new_world: &mut World, occupied_threshold: usize, max_r: i32) {
        static DIRS: [(i32, i32); 8] = [
            (-1, -1),
            (0, -1),
            (1, -1),
            (-1, 0),
            (1, 0),
            (-1, 1),
            (0, 1),
            (1, 1),
        ];

        new_world.clone_from(self);

        for (y, row) in self.seats.enumerated() {
            for (x, &s) in row.enumerated() {
                if s == Seat::Floor {
                    continue;
                }

                let occupied_neighbors = DIRS
                    .iter()
                    .filter(|&(dx, dy)| {
                        for i in 1..=max_r {
                            let x = x + (dx * i);
                            let y = y + (dy * i);

                            match self.seats.get(y).and_then(|r| r.get(x)) {
                                None => break,
                                Some(Seat::Floor) => continue,
                                Some(Seat::Free) => return false,
                                Some(Seat::Occupied) => return true,
                            }
                        }

                        false
                    })
                    .count();

                if s == Seat::Occupied && occupied_neighbors >= occupied_threshold {
                    new_world.seats[y][x] = Seat::Free;
                } else if s == Seat::Free && occupied_neighbors == 0 {
                    new_world.seats[y][x] = Seat::Occupied;
                }
            }
        }
    }

    fn count_occupied(&self) -> usize {
        self.seats
            .iter()
            .map(|row| row.iter().filter(|c| **c == Seat::Occupied).count())
            .sum()
    }
}

impl std::str::FromStr for World {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut seats = IVec::new();

        for l in s.lines() {
            let mut row = IVec::new();

            for c in l.chars() {
                let cell = match c {
                    '.' => Seat::Floor,
                    'L' => Seat::Free,
                    '#' => Seat::Occupied,
                    _ => return Err(()),
                };
                row.push(cell);
            }

            seats.push(row);
        }

        Ok(World { seats })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2470, part1(include_str!("../input/day11.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(2259, part2(include_str!("../input/day11.txt")));
    }
}

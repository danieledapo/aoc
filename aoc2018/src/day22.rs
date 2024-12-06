use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Area {
    regions: Vec<Vec<Region>>,
    target_x: usize,
    target_y: usize,
    depth: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Region {
    erosion_level: usize,
    rtype: RegionType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegionType {
    Rocky,
    Narrow,
    Wet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Equipment {
    Torch,
    ClimbingGear,
    Nothing,
}

pub fn part1(depth: usize, target_x: usize, target_y: usize) -> u32 {
    let area = Area::new(depth, target_x, target_y);

    area.regions
        .iter()
        .flat_map(|r| r.iter())
        .map(|r| match r.rtype {
            RegionType::Rocky => 0,
            RegionType::Wet => 1,
            RegionType::Narrow => 2,
        })
        .sum()
}

pub fn part2(depth: usize, target_x: usize, target_y: usize) -> usize {
    let mut area = Area::new(depth, target_x, target_y);

    // Dijkstra's algorithm
    let mut dists = HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push(Reverse((0, Equipment::Torch, (0, 0))));

    while let Some(Reverse((t, cur_equip, (x, y)))) = queue.pop() {
        match dists.entry((cur_equip, (x, y))) {
            Entry::Vacant(v) => {
                v.insert(t);
            }
            Entry::Occupied(mut o) => {
                if t < *o.get() {
                    *o.get_mut() = t;
                } else {
                    continue;
                }
            }
        };

        if x == target_x && y == target_y && cur_equip == Equipment::Torch {
            return t;
        }

        let mut visit_neighbor = |nx, ny| {
            if area.at(nx, ny).available_equipments().contains(&cur_equip) {
                queue.push(Reverse((t + 1, cur_equip, (nx, ny))));
            }
        };

        visit_neighbor(x + 1, y);
        visit_neighbor(x, y + 1);

        if x > 0 {
            visit_neighbor(x - 1, y);
        }
        if y > 0 {
            visit_neighbor(x, y - 1);
        }

        let r = area.at(x, y);
        for e in r.available_equipments().iter().filter(|e| **e != cur_equip) {
            queue.push(Reverse((t + 7, *e, (x, y))));
        }
    }

    panic!("target is not reachable")
}

impl Area {
    fn new(depth: usize, target_x: usize, target_y: usize) -> Self {
        let mut area = Area {
            regions: vec![],
            target_x,
            target_y,
            depth,
        };

        // prefill base map
        area.at(target_x, target_y);

        area
    }

    fn at(&mut self, x: usize, y: usize) -> Region {
        if let Some(r) = self.regions.get(y).and_then(|r| r.get(x)) {
            return *r;
        }

        let region = match (x, y) {
            (0, 0) => Region::new(self.depth, 0),
            (x, 0) => {
                self.at(x - 1, 0);
                Region::new(self.depth, x * 16807)
            }
            (0, y) => {
                self.at(0, y - 1);
                Region::new(self.depth, y * 48271)
            }
            (x, y) => {
                let above = self.at(x, y - 1);
                let left = self.at(x - 1, y);

                if x == self.target_x && y == self.target_y {
                    Region::new(self.depth, 0)
                } else {
                    Region::new(self.depth, above.erosion_level * left.erosion_level)
                }
            }
        };

        if y >= self.regions.len() {
            self.regions.push(Vec::new());
            assert!(y < self.regions.len());
        }

        assert!(self.regions[y].len() == x);
        self.regions[y].push(region);

        region
    }
}

impl Region {
    fn new(depth: usize, geologic_index: usize) -> Self {
        let erosion_level = (geologic_index + depth) % 20183;

        let rtype = match erosion_level % 3 {
            0 => RegionType::Rocky,
            1 => RegionType::Wet,
            2 => RegionType::Narrow,
            _ => unreachable!(),
        };

        Region {
            erosion_level,
            rtype,
        }
    }

    fn available_equipments(&self) -> [Equipment; 2] {
        match self.rtype {
            RegionType::Rocky => [Equipment::ClimbingGear, Equipment::Torch],
            RegionType::Wet => [Equipment::ClimbingGear, Equipment::Nothing],
            RegionType::Narrow => [Equipment::Torch, Equipment::Nothing],
        }
    }
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for row in &self.regions {
            writeln!(
                f,
                "{}",
                row.iter()
                    .map(|c| format!("{}", c.rtype))
                    .collect::<String>()
            )?;
        }

        Ok(())
    }
}

impl Display for RegionType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c = match self {
            RegionType::Rocky => '.',
            RegionType::Wet => '=',
            RegionType::Narrow => '|',
        };

        write!(f, "{}", c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(7901, part1(6969, 9, 796));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(1087, part2(6969, 9, 796));
    }
}

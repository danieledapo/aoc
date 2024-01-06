use std::{
    cmp::Reverse,
    collections::{BTreeSet, BinaryHeap},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Amphipods {
    id: u8,
    x: i64,
    y: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct World {
    amphipods: BTreeSet<Amphipods>,
    room_depth: i64,
}

pub fn part1(world: &World) -> i64 {
    world.energy_to_organize()
}

impl World {
    pub fn new(room_depth: i64, amphipods: impl IntoIterator<Item = Amphipods>) -> Self {
        Self {
            amphipods: amphipods.into_iter().collect(),
            room_depth,
        }
    }

    pub fn energy_to_organize(&self) -> i64 {
        let mut seen = BTreeSet::new();
        let mut states = BinaryHeap::new();
        states.push((Reverse(0), self.clone()));

        while let Some((Reverse(cost), world)) = states.pop() {
            if world.organized() {
                return cost;
            }

            if !seen.insert(world.clone()) {
                continue;
            }

            for (move_cost, w) in world.moves() {
                states.push((Reverse(cost + move_cost), w));
            }
        }

        unreachable!()
    }

    pub fn moves(&self) -> Vec<(i64, World)> {
        let mut res = vec![];

        for amphi in &self.amphipods {
            if amphi.in_room() {
                // from a room into the hallway if the path is free
                let cannot_leave = self
                    .amphipods
                    .iter()
                    .any(|a| a.x == amphi.x && a.y < amphi.y);

                if cannot_leave {
                    continue;
                }

                // skip spaces right above the exits
                for x in [0, 1, 3, 5, 7, 9, 10] {
                    let (sx, ex) = (x.min(amphi.x), x.max(amphi.x));

                    let cannot_move = self
                        .amphipods
                        .iter()
                        .any(|a| a.in_hallway() && ((sx < a.x && ex > a.x) || a.x == x));
                    if cannot_move {
                        continue;
                    }

                    let mut world = self.clone();
                    world.amphipods.remove(amphi);
                    world.amphipods.insert(Amphipods::new(amphi.id, x, 0));

                    // cost to get out of the room plus horizontal movement
                    let steps = amphi.y + ex - sx;
                    res.push((amphi.energy_for(steps), world));
                }

                continue;
            }

            // amphipod is in the hallway, go to the destination room

            let (sx, ex) = (amphi.room_x().min(amphi.x), amphi.room_x().max(amphi.x));

            let mut y = self.room_depth;
            for a in &self.amphipods {
                if a.in_hallway() {
                    // amphipod is on the hallway path
                    if (sx < a.x && ex > a.x) || a.x == amphi.room_x() {
                        y = 0;
                        break;
                    }

                    continue;
                }

                if a.x != amphi.room_x() {
                    continue;
                }

                // room occupied by amphipods of other types
                if a.id != amphi.id {
                    y = 0;
                    break;
                }

                y = y.min(a.y - 1);
            }

            if y <= 0 {
                continue;
            }

            let mut world = self.clone();
            world.amphipods.remove(amphi);
            world
                .amphipods
                .insert(Amphipods::new(amphi.id, amphi.room_x(), y));

            // cost for the horizontal movement plus to get into the room
            let steps = ex - sx + y;
            res.push((amphi.energy_for(steps), world));
        }

        res
    }

    pub fn organized(&self) -> bool {
        self.amphipods.iter().all(Amphipods::at_destination)
    }
}

impl Amphipods {
    pub fn new(id: u8, x: i64, y: i64) -> Self {
        Self { id, x, y }
    }

    pub fn at_destination(&self) -> bool {
        self.x == self.room_x() && self.in_room()
    }

    pub fn in_room(&self) -> bool {
        self.y > 0
    }

    pub fn in_hallway(&self) -> bool {
        !self.in_room()
    }

    pub fn room_x(&self) -> i64 {
        2 + i64::from(self.id) * 2
    }

    pub fn energy_for(&self, steps: i64) -> i64 {
        steps * 10_i64.pow(self.id.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(
            11516,
            part1(&World::new(
                2,
                vec![
                    // first
                    Amphipods::new(2, 2, 1),
                    Amphipods::new(1, 2, 2),
                    // second
                    Amphipods::new(0, 4, 1),
                    Amphipods::new(0, 4, 2),
                    // third
                    Amphipods::new(1, 6, 1),
                    Amphipods::new(3, 6, 2),
                    // fourth
                    Amphipods::new(3, 8, 1),
                    Amphipods::new(2, 8, 2),
                ],
            ))
        );
    }

    #[test]
    fn test_part2() {
        assert_eq!(
            40272,
            part1(&World::new(
                4,
                vec![
                    // first
                    Amphipods::new(2, 2, 1),
                    Amphipods::new(3, 2, 2),
                    Amphipods::new(3, 2, 3),
                    Amphipods::new(1, 2, 4),
                    // second
                    Amphipods::new(0, 4, 1),
                    Amphipods::new(2, 4, 2),
                    Amphipods::new(1, 4, 3),
                    Amphipods::new(0, 4, 4),
                    // third
                    Amphipods::new(1, 6, 1),
                    Amphipods::new(1, 6, 2),
                    Amphipods::new(0, 6, 3),
                    Amphipods::new(3, 6, 4),
                    // fourth
                    Amphipods::new(3, 8, 1),
                    Amphipods::new(0, 8, 2),
                    Amphipods::new(2, 8, 3),
                    Amphipods::new(2, 8, 4),
                ],
            ))
        );
    }
}

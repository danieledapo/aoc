use std::collections::{BinaryHeap, HashSet};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u16)]
enum Element {
    Cobalt = 1,
    Curium = 2,
    Dilithium = 4,
    Elerium = 8,
    Hydrogen = 16,
    Lithium = 32,
    Plutonium = 64,
    Promethium = 128,
    Ruthenium = 256,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Building {
    floors: [Floor; 4],
    elevator: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Floor {
    microchips: u16,
    generators: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Solution(usize, Building);

pub fn part1() -> usize {
    minimum_steps(Building::input_part1())
}

pub fn part2() -> usize {
    // it's slow, but fast enough in release mode
    minimum_steps(Building::input_part2())
}

fn minimum_steps(building: Building) -> usize {
    let mut queue = BinaryHeap::new();
    queue.push(Solution(0, building));

    let mut seen = HashSet::new();
    while let Some(Solution(steps, building)) = queue.pop() {
        if building.everything_on_last_floor() {
            return steps;
        }

        if !seen.insert(building.clone()) {
            continue;
        }

        for candidate in building.possible_elevator_moves() {
            if !seen.contains(&candidate) {
                queue.push(Solution(steps + 1, candidate));
            }
        }
    }

    unreachable!()
}

impl Building {
    fn everything_on_last_floor(&self) -> bool {
        self.floors
            .iter()
            .take(self.floors.len() - 1)
            .all(Floor::is_empty)
    }

    fn possible_elevator_moves(&self) -> Vec<Building> {
        let mut moves = vec![];

        let floor = &self.floors[self.elevator];

        let mut push_candidates = |new_elevator: usize| {
            // two chips
            for (i, chip1) in floor.iter_chips().enumerate() {
                for chip2 in floor.iter_chips().skip(i) {
                    let mut candidate = self.clone();
                    candidate.floors[new_elevator].insert_chip(chip1);
                    candidate.floors[new_elevator].insert_chip(chip2);
                    candidate.floors[self.elevator].remove_chip(chip1);
                    candidate.floors[self.elevator].remove_chip(chip2);
                    candidate.elevator = new_elevator;

                    if candidate.is_valid() {
                        moves.push(candidate);
                    }
                }
            }

            // two generators
            for (i, gen1) in floor.iter_generators().enumerate() {
                for gen2 in floor.iter_generators().skip(i) {
                    let mut candidate = self.clone();
                    candidate.floors[new_elevator].insert_generator(gen1);
                    candidate.floors[new_elevator].insert_generator(gen2);
                    candidate.floors[self.elevator].remove_generator(gen1);
                    candidate.floors[self.elevator].remove_generator(gen2);
                    candidate.elevator = new_elevator;

                    if candidate.is_valid() {
                        moves.push(candidate);
                    }
                }
            }

            // one chip, one generator
            for (i, chip) in floor.iter_chips().enumerate() {
                for gen in floor.iter_generators().skip(i) {
                    let mut candidate = self.clone();
                    candidate.floors[new_elevator].insert_generator(gen);
                    candidate.floors[new_elevator].insert_chip(chip);
                    candidate.floors[self.elevator].remove_generator(gen);
                    candidate.floors[self.elevator].remove_chip(chip);
                    candidate.elevator = new_elevator;

                    if candidate.is_valid() {
                        moves.push(candidate);
                    }
                }
            }
        };

        // try to first go up
        if self.elevator < self.floors.len() - 1 {
            push_candidates(self.elevator + 1);
        }

        // if the floor below is not empty, try to bring at most one item down
        if self.elevator > 0 {
            push_candidates(self.elevator - 1);
        }

        moves
    }

    fn is_valid(&self) -> bool {
        // there must be at least a microchip in the elevator floor
        if !self.floors[self.elevator].has_chips() {
            return false;
        }

        // all the floors must either have no microchips or all the microchips must have the
        // proper generator in case there are generators on that floor
        self.floors.iter().all(Floor::is_valid)
    }
}

impl Floor {
    pub fn new(
        chips: impl IntoIterator<Item = Element>,
        generators: impl IntoIterator<Item = Element>,
    ) -> Self {
        Self {
            microchips: chips.into_iter().map(|e| e as u16).sum(),
            generators: generators.into_iter().map(|e| e as u16).sum(),
        }
    }

    pub fn iter_chips(&self) -> impl Iterator<Item = Element> {
        Self::iter_u16(self.microchips)
    }
    pub fn iter_generators(&self) -> impl Iterator<Item = Element> {
        Self::iter_u16(self.generators)
    }

    pub fn has_chips(&self) -> bool {
        self.microchips != 0
    }

    pub fn is_valid(&self) -> bool {
        if self.generators == 0 {
            return true;
        }

        (self.generators & self.microchips) == self.microchips
    }

    pub fn insert_chip(&mut self, el: Element) {
        self.microchips |= el as u16;
    }
    pub fn remove_chip(&mut self, el: Element) {
        self.microchips &= !(el as u16);
    }

    pub fn insert_generator(&mut self, el: Element) {
        self.generators |= el as u16;
    }
    pub fn remove_generator(&mut self, el: Element) {
        self.generators &= !(el as u16);
    }

    pub fn is_empty(&self) -> bool {
        self.generators == 0 && self.microchips == 0
    }

    fn iter_u16(n: u16) -> impl Iterator<Item = Element> {
        use Element::*;

        [
            Cobalt, Curium, Dilithium, Elerium, Hydrogen, Lithium, Plutonium, Promethium, Ruthenium,
        ]
        .iter()
        .filter(move |&&e| (n & e as u16) != 0)
        .copied()
    }
}

impl Ord for Solution {
    fn cmp(&self, o: &Solution) -> std::cmp::Ordering {
        self.partial_cmp(o).unwrap()
    }
}
impl PartialOrd for Solution {
    fn partial_cmp(&self, o: &Solution) -> Option<std::cmp::Ordering> {
        // reverse order as we want the heap to be a min heap
        o.0.partial_cmp(&self.0)
    }
}

impl Building {
    pub fn input_part1() -> Self {
        Building {
            floors: [
                Floor::new(Some(Element::Promethium), Some(Element::Promethium)),
                Floor::new(
                    None,
                    vec![
                        Element::Cobalt,
                        Element::Curium,
                        Element::Ruthenium,
                        Element::Plutonium,
                    ],
                ),
                Floor::new(
                    vec![
                        Element::Cobalt,
                        Element::Curium,
                        Element::Ruthenium,
                        Element::Plutonium,
                    ],
                    None,
                ),
                Floor::new(None, None),
            ],
            elevator: 0,
        }
    }

    pub fn input_part2() -> Self {
        Building {
            floors: [
                Floor::new(
                    vec![Element::Elerium, Element::Dilithium, Element::Promethium],
                    vec![Element::Elerium, Element::Dilithium, Element::Promethium],
                ),
                Floor::new(
                    None,
                    vec![
                        Element::Cobalt,
                        Element::Curium,
                        Element::Ruthenium,
                        Element::Plutonium,
                    ],
                ),
                Floor::new(
                    vec![
                        Element::Cobalt,
                        Element::Curium,
                        Element::Ruthenium,
                        Element::Plutonium,
                    ],
                    None,
                ),
                Floor::new(None, None),
            ],
            elevator: 0,
        }
    }

    pub fn example() -> Self {
        Building {
            floors: [
                Floor::new(vec![Element::Hydrogen, Element::Lithium], None),
                Floor::new(None, Some(Element::Hydrogen)),
                Floor::new(None, Some(Element::Lithium)),
                Floor::new(None, None),
            ],
            elevator: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(minimum_steps(Building::example()), 11);
        assert_eq!(part1(), 33);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 57);
    }
}

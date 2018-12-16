use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

// row, column
type Pos = (usize, usize);

#[derive(Debug, Clone)]
struct Game {
    walls: HashSet<Pos>,
    units: HashMap<Pos, Unit>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GameState {
    End,
    InProgress,
    ElfDied,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Unit {
    position: Pos,
    health: u8,
    power: u8,
    team: Team,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Team {
    Elf,
    Goblin,
}

pub fn part1(input: &str) -> u32 {
    let mut game = Game::from_str(3, input).unwrap();

    for i in 0.. {
        // println!("T: {}", i);
        // println!("{}", game);

        if game.play_next_round(false) == GameState::End {
            return game.outcome(i);
        }
    }

    unreachable!("eventually someone has to die")
}

pub fn part2(input: &str) -> u32 {
    for power in 4.. {
        let mut game = Game::from_str(power, input).unwrap();

        for i in 0.. {
            match game.play_next_round(true) {
                GameState::ElfDied => break,
                GameState::End => {
                    // println!("Power: {} T: {}", power, i);
                    // println!("{}", game);
                    return game.outcome(i);
                }
                GameState::InProgress => {}
            };
        }
    }

    unreachable!("eventually elves have to win")
}

impl Game {
    fn outcome(&self, i: u32) -> u32 {
        i * self
            .units
            .values()
            .map(|u| u32::from(u.health))
            .sum::<u32>()
    }

    fn play_next_round(&mut self, break_on_elf_death: bool) -> GameState {
        let units_pos = self.units.keys().cloned().collect::<BTreeSet<_>>();

        for unit_pos in units_pos {
            if !self.units.contains_key(&unit_pos) {
                continue;
            }

            let targets = self
                .units
                .values()
                .filter(|u| u.team != self.units[&unit_pos].team && u.position != unit_pos)
                .cloned()
                .collect::<Vec<_>>();

            if targets.is_empty() {
                return GameState::End;
            }

            let mut in_range = Vec::with_capacity(targets.len() * 4);
            let mut can_move = true;
            for t in targets {
                for pos in &neighbors(&t.position) {
                    if *pos == unit_pos {
                        can_move = false;
                        break;
                    }

                    if !self.walls.contains(pos) && !self.units.contains_key(pos) {
                        in_range.push(*pos);
                    }
                }
            }

            let moved_unit_pos = if can_move {
                let old_unit = self.units.remove(&unit_pos).unwrap();
                let moved_unit = self.move_unit(old_unit, in_range);

                let already_present = self.units.insert(moved_unit.position, moved_unit.clone());
                assert!(already_present.is_none());

                moved_unit.position
            } else {
                unit_pos
            };

            let gs = self.unit_attack(&self.units[&moved_unit_pos].clone());
            if break_on_elf_death && gs == GameState::ElfDied {
                return gs;
            }
        }

        GameState::InProgress
    }

    fn move_unit(&self, unit: Unit, targets: Vec<Pos>) -> Unit {
        let mut dists = HashMap::new();
        let mut to_visit = VecDeque::new();
        to_visit.push_back((unit.position, 0));

        while !to_visit.is_empty() {
            let (seed_pos, seed_dist) = to_visit.pop_front().unwrap();

            for pos in &neighbors(&seed_pos) {
                if self.walls.contains(&pos) || self.units.contains_key(&pos) {
                    continue;
                }

                let dist = seed_dist + 1;

                match dists.entry(*pos) {
                    Entry::Vacant(v) => {
                        v.insert((dist, seed_pos));
                    }
                    Entry::Occupied(mut o) => {
                        if (dist, seed_pos) < *o.get() {
                            *o.get_mut() = (dist, seed_pos);
                        } else {
                            continue;
                        }
                    }
                };

                to_visit.push_back((*pos, dist));
            }
        }

        let closest_enemy = targets
            .into_iter()
            .flat_map(|pos| dists.get(&pos).map(move |d| (d, pos)))
            .min();

        if let Some((_, mut closest_enemy_pos)) = closest_enemy {
            while dists[&closest_enemy_pos].0 != 1 {
                closest_enemy_pos = dists[&closest_enemy_pos].1;
            }

            if !self.units.contains_key(&closest_enemy_pos) {
                return Unit {
                    position: closest_enemy_pos,
                    ..unit
                };
            }
        }

        unit
    }

    fn unit_attack(&mut self, attacker: &Unit) -> GameState {
        let neighbors = neighbors(&attacker.position);
        let closest_enemy_pos = neighbors
            .iter()
            .filter(|pos| {
                self.units
                    .get(pos)
                    .map_or(false, |u| u.team != attacker.team)
            })
            .min_by_key(|pos| (self.units[pos].health, *pos));

        if let Some(enemy_pos) = closest_enemy_pos {
            match self.units.entry(*enemy_pos) {
                Entry::Vacant(_) => unreachable!(),
                Entry::Occupied(mut o) => {
                    if o.get().health <= attacker.power {
                        if o.remove().team == Team::Elf {
                            return GameState::ElfDied;
                        }
                    } else {
                        o.get_mut().health -= attacker.power;
                    }
                }
            }
        }

        GameState::InProgress
    }

    fn from_str(elf_power: u8, input: &str) -> Result<Self, char> {
        let mut game = Game {
            walls: HashSet::new(),
            units: HashMap::new(),
        };

        for (y, l) in input.lines().enumerate() {
            for (x, c) in l.chars().enumerate() {
                match c {
                    '#' => {
                        game.walls.insert((y, x));
                    }
                    '.' => {}
                    c if c == 'E' || c == 'G' => {
                        game.units.insert(
                            (y, x),
                            Unit {
                                position: (y, x),
                                health: 200,
                                power: if c == 'G' { 3 } else { elf_power },
                                team: if c == 'G' { Team::Goblin } else { Team::Elf },
                            },
                        );
                    }
                    c => return Err(c),
                };
            }
        }

        Ok(game)
    }
}

fn neighbors(p: &Pos) -> [Pos; 4] {
    // we can ignore checking for out of bounds because the map is always surrounded by walls and
    // this function does not get called on them.
    //
    // in reading order
    [
        (p.0 - 1, p.1),
        (p.0, p.1 - 1),
        (p.0, p.1 + 1),
        (p.0 + 1, p.1),
    ]
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let max_x = *self.walls.iter().map(|(_, x)| x).max().unwrap();
        let max_y = *self.walls.iter().map(|(y, _)| y).max().unwrap();

        for y in 0..=max_y {
            let row = (0..=max_x)
                .map(move |x| {
                    if self.walls.contains(&(y, x)) {
                        return '#';
                    }

                    match self.units.get(&(y, x)) {
                        None => '.',
                        Some(u) => {
                            if u.team == Team::Elf {
                                'E'
                            } else {
                                'G'
                            }
                        }
                    }
                })
                .collect::<String>();

            let units = (0..=max_x)
                .flat_map(move |x| self.units.get(&(y, x)))
                .map(|u| {
                    let prefix = if u.team == Team::Elf { 'E' } else { 'G' };
                    format!("{}({})", prefix, u.health)
                })
                .collect::<Vec<String>>()
                .join(", ");

            writeln!(f, "{}   {}", row, units)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(
            27730,
            part1(
                r"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"
            )
        );
        assert_eq!(
            27755,
            part1(
                r"#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"
            )
        );
        assert_eq!(
            28944,
            part1(
                r"#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"
            )
        );

        assert_eq!(
            36334,
            part1(
                r"#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"
            )
        );
        assert_eq!(189000, part1(include_str!("../input/day15.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(
            4988,
            part2(
                r"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"
            )
        );
        assert_eq!(
            31284,
            part2(
                r"#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"
            )
        );
        assert_eq!(
            6474,
            part2(
                r"#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"
            )
        );
        assert_eq!(
            1140,
            part2(
                r"#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"
            )
        );
        assert_eq!(38512, part2(include_str!("../input/day15.txt")));
    }
}

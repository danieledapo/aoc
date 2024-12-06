use std::cmp::Reverse;
use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Reindeer<'a> {
    groups: Vec<Group<'a>>,
}

#[derive(Debug, Clone)]
struct Group<'a> {
    faction: Faction,
    units: u32,
    hp: u32,
    damage: u32,
    attack_type: &'a str,
    weaknesses: HashSet<&'a str>,
    immunities: HashSet<&'a str>,
    initiative: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Faction {
    Immune,
    Infection,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FightState {
    Stall,
    InProgress,
}

pub fn part1(input: &str) -> u32 {
    let mut reindeer = Reindeer::parse(input).unwrap();

    while reindeer.fight() == FightState::InProgress {
        // keep fighting
    }

    match (
        reindeer.alive_units(Faction::Immune),
        reindeer.alive_units(Faction::Infection),
    ) {
        (0, infections) => infections,
        (immune, 0) => immune,
        _ => unreachable!("somebody died"),
    }
}

pub fn part2(input: &str) -> u32 {
    for b in 0.. {
        let mut reindeer = Reindeer::parse(input).unwrap();
        reindeer.boost(b);

        while reindeer.fight() == FightState::InProgress {
            // keep fighting
        }

        if reindeer.alive_units(Faction::Infection) == 0 {
            return reindeer.alive_units(Faction::Immune);
        }
    }

    unreachable!("eventually the immune system has to win")
}

impl<'a> Reindeer<'a> {
    fn boost(&mut self, boost: u32) {
        self.groups
            .iter_mut()
            .filter(|g| g.faction == Faction::Immune)
            .for_each(|g| g.damage += boost);
    }

    fn fight(&mut self) -> FightState {
        self.groups
            .sort_by_key(|g| Reverse((g.effective_power(), g.initiative)));

        let mut selections = self.target_selection();

        selections.sort_by_key(|(src, _)| Reverse(self.groups[*src].initiative));

        let state = self.attack(selections);

        self.groups = self
            .groups
            .iter()
            .filter(|g| g.units > 0)
            .cloned()
            .collect::<Vec<_>>();

        state
    }

    fn target_selection(&self) -> Vec<(usize, usize)> {
        let mut available_targets = (0..self.groups.len()).collect::<HashSet<_>>();
        let mut selections = Vec::with_capacity(self.groups.len());

        for (i, g) in self.groups.iter().enumerate() {
            let target_ix = available_targets
                .iter()
                .filter(|i| self.groups[**i].faction != g.faction)
                .max_by_key(|i| {
                    let tg = &self.groups[**i];

                    (g.calc_damage(tg), tg.effective_power(), tg.initiative)
                })
                .cloned();

            if let Some(target) = target_ix {
                if g.calc_damage(&self.groups[target]) > 0 {
                    selections.push((i, target));
                    available_targets.remove(&target);
                }
            }
        }

        selections
    }

    fn attack(&mut self, selections: Vec<(usize, usize)>) -> FightState {
        let mut units_killed = false;

        for (src, target) in selections {
            if self.groups[src].units == 0 {
                continue;
            }

            let damage = self.groups[src].calc_damage(&self.groups[target]);
            let units_lost = damage / self.groups[target].hp;

            if units_lost > 0 {
                units_killed = true;
            }

            if self.groups[target].units <= units_lost {
                self.groups[target].units = 0;
            } else {
                self.groups[target].units -= units_lost;
            }
        }

        if units_killed {
            FightState::InProgress
        } else {
            FightState::Stall
        }
    }

    fn alive_units(&self, faction: Faction) -> u32 {
        self.groups
            .iter()
            .filter(|u| u.faction == faction)
            .map(|u| u.units)
            .sum()
    }

    fn parse(input: &'a str) -> Option<Self> {
        let mut parts = input.trim().split("\n\n");

        let parse_system = |s: &'a str| {
            let mut lines = s.lines();

            let faction = match lines.next()?.trim_end_matches(':') {
                "Immune System" => Faction::Immune,
                "Infection" => Faction::Infection,
                _ => return None,
            };

            Some(lines.map(move |l| Group::parse(faction, l)))
        };

        let mut groups = parse_system(parts.next()?)?.collect::<Option<Vec<_>>>()?;
        groups.extend(parse_system(parts.next()?)?.collect::<Option<Vec<_>>>()?);

        Some(Reindeer { groups })
    }
}

impl<'a> Group<'a> {
    fn calc_damage(&self, target: &Group<'a>) -> u32 {
        if target.immunities.contains(self.attack_type) {
            return 0;
        }

        let base_damage = self.effective_power();

        if target.weaknesses.contains(self.attack_type) {
            return base_damage * 2;
        }

        base_damage
    }

    fn effective_power(&self) -> u32 {
        self.units * self.damage
    }

    fn parse(faction: Faction, input: &'a str) -> Option<Self> {
        let parse_num = |s: &'a str| s.parse::<u32>().ok();

        let mut part1 = (&input[0..input.find("hit points")?]).split_whitespace();
        let units = parse_num(part1.next()?)?;
        let hp = parse_num(part1.nth(3)?)?;

        let mut weaknesses = HashSet::new();
        let mut immunities = HashSet::new();

        if let (Some(lparen), Some(rparen)) = (input.find('('), input.find(')')) {
            let modifiers = &input[lparen + 1..rparen];

            for m in modifiers.split("; ") {
                let m = m.trim();

                let (mr, m) = if m.starts_with("immune to ") {
                    (&mut immunities, m.trim_start_matches("immune to "))
                } else {
                    (&mut weaknesses, m.trim_start_matches("weak to "))
                };

                mr.extend(m.split(", "));
            }
        };

        let part2 = &input[input.find("with an attack")?..];
        let mut part2 = part2.split_whitespace().skip(5);
        let damage = parse_num(part2.next()?)?;
        let attack_type = part2.next()?;
        let initiative = parse_num(part2.nth(3)?)?;

        Some(Group {
            units,
            hp,
            damage,
            attack_type,
            initiative,
            weaknesses,
            immunities,
            faction,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(15493, part1(include_str!("../input/day24.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(1045, part2(include_str!("../input/day24.txt")));
    }
}

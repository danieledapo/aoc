#[derive(Debug, Clone, PartialEq, Eq)]
struct Role {
    damage: u8,
    armor: u8,
    hp: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Item {
    name: &'static str,
    cost: u32,
    damage: u8,
    armor: u8,
}

#[rustfmt::skip]
static WEAPONS: [Item; 5] = [
    Item { name: "Dagger", cost: 8,  damage: 4,  armor: 0 },
    Item { name: "Shortsword", cost: 10, damage: 5, armor: 0 },
    Item { name: "Warhammer", cost: 25, damage: 6, armor: 0 },
    Item { name: "Longsword", cost: 40, damage: 7, armor: 0 },
    Item { name: "Greataxe", cost: 74, damage: 8, armor: 0 },
];

#[rustfmt::skip]
static ARMORS: [Item; 5] = [
    Item { name: "Leather", cost: 13, damage: 0, armor: 1 },
    Item { name: "Chainmail", cost: 31, damage: 0, armor: 2 },
    Item { name: "Splintmail", cost: 53, damage: 0, armor: 3 },
    Item { name: "Bandedmail", cost: 75, damage: 0, armor: 4 },
    Item { name: "Platemail", cost: 102, damage: 0, armor: 5 },
];

#[rustfmt::skip]
static RINGS: [Item; 6] = [
    Item { name: "Damage +1", cost: 25, damage: 1, armor: 0 },
    Item { name: "Damage +2", cost: 50, damage: 2, armor: 0 },
    Item { name: "Damage +3", cost: 100, damage: 3, armor: 0 },
    Item { name: "Defense +1", cost: 20, damage: 0, armor: 1 },
    Item { name: "Defense +2", cost: 40, damage: 0, armor: 2 },
    Item { name: "Defense +3", cost: 80, damage: 0, armor: 3 },
];

#[derive(Debug, Clone, PartialEq, Eq)]
enum FightResult {
    PlayerWon,
    BossWon,
}

static BASE_PLAYER: Role = Role {
    hp: 100,
    damage: 0,
    armor: 0,
};

static BOSS: Role = Role {
    hp: 100,
    damage: 8,
    armor: 2,
};

pub fn part1() -> u32 {
    powerups(&BASE_PLAYER)
        .filter_map(|(gold, player)| {
            if fight(player, BOSS.clone()) == FightResult::PlayerWon {
                Some(gold)
            } else {
                None
            }
        })
        .min()
        .unwrap()
}

pub fn part2() -> u32 {
    powerups(&BASE_PLAYER)
        .filter_map(|(gold, player)| {
            if fight(player, BOSS.clone()) == FightResult::BossWon {
                Some(gold)
            } else {
                None
            }
        })
        .max()
        .unwrap()
}

fn fight(mut player: Role, mut boss: Role) -> FightResult {
    let attack = |attacker: &Role, defender: &mut Role| {
        defender.hp = defender
            .hp
            .saturating_sub((attacker.damage.saturating_sub(defender.armor)).max(1));
    };

    loop {
        attack(&player, &mut boss);
        if boss.hp == 0 {
            return FightResult::PlayerWon;
        }

        attack(&boss, &mut player);
        if player.hp == 0 {
            return FightResult::BossWon;
        }
    }
}

fn powerups<'a>(base_player: &'a Role) -> impl Iterator<Item = (u32, Role)> + 'a {
    // add 1 weapon
    let with_weapons = WEAPONS
        .iter()
        .map(move |w| (w.cost, base_player.powerup(w)));

    // add at most 1 armor
    let with_armors = with_weapons.flat_map(|(cost, player)| {
        let p = player.clone();
        ARMORS
            .iter()
            .map(move |a| (cost + a.cost, player.powerup(a)))
            .chain(std::iter::once((cost, p)))
    });

    // collect into a Vec so that we break the long type and rustc doesn't
    // complain. This happens because of the long chain of flat_map, chain,
    // once, etc... This is annoying...
    let with_armors = with_armors.collect::<Vec<_>>().into_iter();

    // add at most 1 ring
    let with_1_ring = with_armors.flat_map(|(cost, player)| {
        let p = player.clone();
        RINGS
            .iter()
            .map(move |r| (cost + r.cost, player.powerup(r)))
            .chain(std::iter::once((cost, p)))
    });

    // add at most another ring(2 hands, 2 rings)
    with_1_ring.flat_map(|(cost, player)| {
        let p = player.clone();
        RINGS
            .iter()
            .map(move |r| (cost + r.cost, player.powerup(r)))
            .chain(std::iter::once((cost, p)))
    })
}

impl Role {
    fn powerup(&self, item: &Item) -> Self {
        Role {
            hp: self.hp,
            armor: self.armor + item.armor,
            damage: self.damage + item.damage,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(91, part1());
    }

    #[test]
    fn solution_part2() {
        assert_eq!(158, part2());
    }
}

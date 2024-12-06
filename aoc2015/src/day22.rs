use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    boss: Boss,
    player: Player,
    active_spells: HashMap<Spell, u16>,

    mana_spent: u32,
    min_mana_spent_to_win: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GameState {
    PlayerWon { mana_spent: u32 },
    BossWon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Boss {
    hp: u16,
    damage: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Player {
    hp: u16,
    mana: u32,
    armor: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Spell {
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge,
}

static BOSS: Boss = Boss { hp: 58, damage: 9 };
static START_PLAYER: Player = Player {
    hp: 50,
    mana: 500,
    armor: 0,
};

pub fn part1() -> u32 {
    let mut game = Game {
        boss: BOSS.clone(),
        player: START_PLAYER.clone(),
        active_spells: HashMap::new(),

        min_mana_spent_to_win: u32::max_value(),
        mana_spent: 0,
    };

    match game.fight(0) {
        GameState::PlayerWon { mana_spent } => mana_spent,
        GameState::BossWon => unreachable!("eventually player has to win"),
    }
}

pub fn part2() -> u32 {
    let mut game = Game {
        boss: BOSS.clone(),
        player: START_PLAYER.clone(),
        active_spells: HashMap::new(),

        min_mana_spent_to_win: u32::max_value(),
        mana_spent: 0,
    };

    match game.fight(1) {
        GameState::PlayerWon { mana_spent } => mana_spent,
        GameState::BossWon => unreachable!("eventually player has to win"),
    }
}

impl Game {
    fn fight(&mut self, player_damage_per_turn: u16) -> GameState {
        use self::Spell::*;

        self.player.hp = self.player.hp.saturating_sub(player_damage_per_turn);
        if self.player.hp == 0 {
            return GameState::BossWon;
        }

        self.run_effects();
        if self.boss.hp == 0 {
            return GameState::PlayerWon {
                mana_spent: self.mana_spent,
            };
        }

        let castable_spells = [MagicMissile, Drain, Shield, Poison, Recharge]
            .iter()
            .filter(|s| s.mana_cost() <= self.player.mana && !self.active_spells.contains_key(s))
            .collect::<Vec<_>>();

        if castable_spells.is_empty() {
            return GameState::BossWon;
        }

        for spell in castable_spells {
            if self.mana_spent + spell.mana_cost() >= self.min_mana_spent_to_win {
                continue;
            }

            let mut new_game = self.clone();
            new_game.mana_spent += spell.mana_cost();
            new_game.player.mana -= spell.mana_cost();

            if spell.is_effect() {
                new_game
                    .active_spells
                    .insert(spell.clone(), spell.duration());
            } else {
                spell.cast(&mut new_game.player, &mut new_game.boss);
            }

            if new_game.boss.hp > 0 {
                new_game.player.hp = new_game.player.hp.saturating_sub(player_damage_per_turn);

                if new_game.player.hp == 0 {
                    continue;
                }
            }

            if new_game.boss.hp > 0 {
                new_game.run_effects();
            }

            if new_game.boss.hp > 0 {
                new_game.boss_round();

                if new_game.player.hp == 0 {
                    continue;
                }
            }

            if let GameState::PlayerWon { mana_spent } = new_game.fight(player_damage_per_turn) {
                self.min_mana_spent_to_win = self.min_mana_spent_to_win.min(mana_spent);
            }
        }

        GameState::PlayerWon {
            mana_spent: self.min_mana_spent_to_win,
        }
    }

    fn run_effects(&mut self) {
        self.player.armor = 0;

        for (spell, duration) in self.active_spells.iter_mut() {
            spell.cast(&mut self.player, &mut self.boss);
            *duration -= 1;
        }

        self.active_spells.retain(|_, duration| *duration > 0);
    }

    fn boss_round(&mut self) {
        self.player.hp = self
            .player
            .hp
            .saturating_sub((self.boss.damage.saturating_sub(self.player.armor)).max(1));
    }
}

impl Spell {
    fn mana_cost(&self) -> u32 {
        match self {
            Spell::MagicMissile => 53,
            Spell::Drain => 73,
            Spell::Shield => 113,
            Spell::Poison => 173,
            Spell::Recharge => 229,
        }
    }

    fn is_effect(&self) -> bool {
        self.duration() > 0
    }

    fn duration(&self) -> u16 {
        match self {
            Spell::MagicMissile => 0,
            Spell::Drain => 0,
            Spell::Shield => 6,
            Spell::Poison => 6,
            Spell::Recharge => 5,
        }
    }

    fn cast(&self, player: &mut Player, boss: &mut Boss) {
        match self {
            Spell::MagicMissile => boss.hp = boss.hp.saturating_sub(4),
            Spell::Drain => {
                boss.hp = boss.hp.saturating_sub(2);
                player.hp += 2;
            }
            Spell::Shield => {
                player.armor = 7;
            }
            Spell::Poison => {
                boss.hp = boss.hp.saturating_sub(3);
            }
            Spell::Recharge => {
                player.mana += 101;
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(1269, part1());
    }

    #[test]
    fn solution_part2() {
        assert_eq!(1309, part2());
    }
}

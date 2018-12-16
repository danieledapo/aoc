use std::collections::{BTreeSet, HashMap};
use std::str::FromStr;

struct Automata {
    // only pots with plants are stored in current_gen and each element is the
    // position of the pot.
    current_gen: BTreeSet<i64>,
    rules: HashMap<Pattern, CellState>,
}

type Pattern = (CellState, CellState, CellState, CellState, CellState);

#[derive(Debug, PartialEq, Eq, Hash)]
enum CellState {
    On,
    Off,
}

pub fn part1(input: &str) -> i64 {
    let automata: Automata = input.parse().unwrap();

    (0..20).fold(automata, |a, _| a.advance()).plants_sum()
}

pub fn part2(input: &str) -> i64 {
    // the pattern repeats after a while, interpolate and hope for the best
    const CYCLE_PERIOD: usize = 1000;

    let mut automata: Automata = input.parse().unwrap();

    let mut p = 0;
    let mut n = 0;

    for _ in 0..CYCLE_PERIOD {
        p = n;
        automata = automata.advance();
        n = automata.plants_sum();
    }

    p + (n - p) * (50_000_000_000 - CYCLE_PERIOD as i64 + 1)
}

impl Automata {
    fn plants_sum(&self) -> i64 {
        self.current_gen.iter().cloned().sum::<i64>()
    }

    fn state_at(&self, ix: i64) -> CellState {
        if self.current_gen.contains(&ix) {
            CellState::On
        } else {
            CellState::Off
        }
    }

    fn advance(self) -> Self {
        let mut gen = BTreeSet::new();

        let min_ix = self.current_gen.iter().next().unwrap();
        let max_ix = self.current_gen.iter().rev().next().unwrap();

        for i in min_ix - 2..=max_ix + 2 {
            let pattern = (
                self.state_at(i - 2),
                self.state_at(i - 1),
                self.state_at(i),
                self.state_at(i + 1),
                self.state_at(i + 2),
            );

            if *self.rules.get(&pattern).unwrap_or(&CellState::Off) == CellState::On {
                gen.insert(i);
            }
        }

        Automata {
            current_gen: gen,
            rules: self.rules,
        }
    }
}

impl FromStr for Automata {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = input.trim_start_matches("initial state: ");
        let mut lines = input.lines();

        let first_gen = lines
            .next()
            .ok_or(())?
            .chars()
            .enumerate()
            .filter_map(|(i, c)| {
                let cs = CellState::from_char(c)?;

                if cs == CellState::On {
                    Some(Some(i as i64))
                } else {
                    None
                }
            })
            .collect::<Option<BTreeSet<_>>>();
        let first_gen = first_gen.ok_or(())?;

        // eat empty line
        lines.next();

        let rules = lines
            .map(|rule| {
                let mut parts = rule.split(" => ");
                let pattern_inp = parts.next()?;
                let mut pattern_inp = pattern_inp.chars();
                let t1 = CellState::from_char(pattern_inp.next()?)?;
                let t2 = CellState::from_char(pattern_inp.next()?)?;
                let t3 = CellState::from_char(pattern_inp.next()?)?;
                let t4 = CellState::from_char(pattern_inp.next()?)?;
                let t5 = CellState::from_char(pattern_inp.next()?)?;
                let pattern = (t1, t2, t3, t4, t5);

                let result = CellState::from_char(parts.next()?.chars().next()?)?;

                Some((pattern, result))
            })
            .collect::<Option<HashMap<_, _>>>();
        let rules = rules.ok_or(())?;

        Ok(Automata {
            current_gen: first_gen,
            rules,
        })
    }
}

impl CellState {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '#' => Some(CellState::On),
            '.' => Some(CellState::Off),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(2040, part1(include_str!("../input/day12.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(1_700_000_000_011, part2(include_str!("../input/day12.txt")));
    }
}

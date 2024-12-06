use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Area {
    acres: Vec<Vec<Acre>>,
}

#[derive(Debug)]
pub struct AreaPrinter<'a> {
    pub area: &'a Area,
    pub with_colors: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Acre {
    OpenGround,
    Trees,
    Lumberyard,
}

pub fn part1(input: &str) -> usize {
    grow(input, 10)
}

pub fn part2(input: &str) -> usize {
    grow(input, 1_000_000_000)
}

fn grow(input: &str, generations: usize) -> usize {
    let mut cache = HashMap::new();
    let mut state = input.parse::<Area>().unwrap();

    for gen_id in 0..generations {
        // eventually the system reaches a state that continuously goes from a
        // configuration to a given one. In that case we can fast forward by the
        // cycle count and only evolve the state the remaining times.
        if let Some(start_cycle) = cache.get(&state) {
            let remaining = (generations - gen_id) % (gen_id - start_cycle);

            state = (0..remaining).fold(state, |s, _| s.evolve());
            break;
        }

        let evolved = state.evolve();
        cache.insert(state, gen_id);
        state = evolved;
    }

    let (n_trees, n_lumberyard) =
        state
            .acres
            .iter()
            .flat_map(|acs| acs.iter())
            .fold((0, 0), |(nt, nl), acre| match acre {
                Acre::Trees => (nt + 1, nl),
                Acre::Lumberyard => (nt, nl + 1),
                Acre::OpenGround => (nt, nl),
            });

    n_trees * n_lumberyard
}

impl Area {
    pub fn evolve(&self) -> Self {
        let new_acres = (0..self.acres.len())
            .map(|y| {
                (0..self.acres[y].len())
                    .map(|x| self.new_gen(y, x))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        Area { acres: new_acres }
    }

    fn new_gen(&self, y: usize, x: usize) -> Acre {
        let neighbors = self.nb8(y, x);

        let current = self.acres[y][x];

        match current {
            Acre::OpenGround => {
                if neighbors.iter().filter(|a| **a == Acre::Trees).count() >= 3 {
                    Acre::Trees
                } else {
                    Acre::OpenGround
                }
            }
            Acre::Trees => {
                if neighbors.iter().filter(|a| **a == Acre::Lumberyard).count() >= 3 {
                    Acre::Lumberyard
                } else {
                    Acre::Trees
                }
            }
            Acre::Lumberyard => {
                if neighbors.iter().filter(|a| **a == Acre::Lumberyard).count() >= 1
                    && neighbors.iter().filter(|a| **a == Acre::Trees).count() >= 1
                {
                    Acre::Lumberyard
                } else {
                    Acre::OpenGround
                }
            }
        }
    }

    fn nb8(&self, y: usize, x: usize) -> [Acre; 8] {
        [
            if y > 0 && x > 0 {
                self.acres[y - 1][x - 1]
            } else {
                Acre::OpenGround
            },
            if y > 0 {
                self.acres[y - 1][x]
            } else {
                Acre::OpenGround
            },
            if y > 0 && x + 1 < self.acres[y - 1].len() {
                self.acres[y - 1][x + 1]
            } else {
                Acre::OpenGround
            },
            if x > 0 {
                self.acres[y][x - 1]
            } else {
                Acre::OpenGround
            },
            if x + 1 < self.acres[y].len() {
                self.acres[y][x + 1]
            } else {
                Acre::OpenGround
            },
            if y + 1 < self.acres.len() && x > 0 {
                self.acres[y + 1][x - 1]
            } else {
                Acre::OpenGround
            },
            if y + 1 < self.acres.len() {
                self.acres[y + 1][x]
            } else {
                Acre::OpenGround
            },
            if y + 1 < self.acres.len() && x + 1 < self.acres[y + 1].len() {
                self.acres[y + 1][x + 1]
            } else {
                Acre::OpenGround
            },
        ]
    }
}

impl FromStr for Area {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let acres = input
            .lines()
            .map(|l| l.chars().map(Acre::from_char).collect::<Option<Vec<_>>>())
            .collect::<Option<Vec<_>>>()
            .ok_or(())?;

        Ok(Area { acres })
    }
}

impl Acre {
    fn from_char(input: char) -> Option<Self> {
        match input {
            '.' => Some(Acre::OpenGround),
            '|' => Some(Acre::Trees),
            '#' => Some(Acre::Lumberyard),
            _ => None,
        }
    }

    fn symbol(self) -> (char, termion::color::AnsiValue) {
        match self {
            Acre::OpenGround => ('.', termion::color::AnsiValue(0)),
            Acre::Trees => ('|', termion::color::AnsiValue(2)),
            Acre::Lumberyard => ('#', termion::color::AnsiValue(215)),
        }
    }
}

impl Display for AreaPrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for row in &self.area.acres {
            let row = row.iter().map(|a| a.symbol());

            let row: String = if self.with_colors {
                row.map(|(ch, co)| {
                    format!(
                        "{}{}{}",
                        termion::color::Fg(co),
                        ch,
                        termion::color::Fg(termion::color::Reset)
                    )
                })
                .collect()
            } else {
                row.map(|(c, _)| c).collect()
            };

            writeln!(f, "{}", row)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(531_417, part1(include_str!("../input/day18.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(205_296, part2(include_str!("../input/day18.txt")));
    }
}

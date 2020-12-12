use std::str::FromStr;

#[derive(Clone)]
pub enum Action {
    N(i32),
    S(i32),
    E(i32),
    W(i32),
    L(i32),
    R(i32),
    F(i32),
}

pub fn part1(input: &str) -> i32 {
    let mut pos = (0, 0);
    let mut d = (1, 0);

    for a in parse(input) {
        match a {
            Action::N(a) => pos.1 += a,
            Action::S(a) => pos.1 -= a,

            Action::E(a) => pos.0 += a,
            Action::W(a) => pos.0 -= a,

            Action::L(a) => {
                for _ in 0..a / 90 {
                    d = (-d.1, d.0);
                }
            }
            Action::R(a) => {
                for _ in 0..a / 90 {
                    d = (d.1, -d.0);
                }
            }

            Action::F(a) => pos = (pos.0 + a * d.0, pos.1 + a * d.1),
        }
    }

    pos.0.abs() + pos.1.abs()
}

pub fn part2(input: &str) -> i32 {
    let mut ship = (0, 0);
    let mut waypoint = (10, 1);

    for a in parse(input) {
        match a {
            Action::N(a) => waypoint.1 += a,
            Action::S(a) => waypoint.1 -= a,

            Action::E(a) => waypoint.0 += a,
            Action::W(a) => waypoint.0 -= a,

            Action::L(a) => {
                for _ in 0..a / 90 {
                    waypoint = (-waypoint.1, waypoint.0);
                }
            }
            Action::R(a) => {
                for _ in 0..a / 90 {
                    waypoint = (waypoint.1, -waypoint.0);
                }
            }

            Action::F(a) => ship = (ship.0 + a * waypoint.0, ship.1 + a * waypoint.1),
        }
    }

    ship.0.abs() + ship.1.abs()
}

fn parse(input: &str) -> impl Iterator<Item = Action> + '_ {
    input.lines().map(|l| l.parse().unwrap())
}

impl FromStr for Action {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(());
        }

        let n = s[1..].parse().map_err(|_| ())?;
        match &s[0..1] {
            "N" => Ok(Action::N(n)),
            "S" => Ok(Action::S(n)),
            "E" => Ok(Action::E(n)),
            "W" => Ok(Action::W(n)),
            "L" => Ok(Action::L(n)),
            "R" => Ok(Action::R(n)),
            "F" => Ok(Action::F(n)),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2458, part1(include_str!("../input/day12.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(145117, part2(include_str!("../input/day12.txt")));
    }
}

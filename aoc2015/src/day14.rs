#[derive(Debug, Clone, PartialEq, Eq)]
struct Reindeer<'a> {
    name: &'a str,
    velocity: u32,
    run_period: u32,
    rest_period: u32,

    traveled_distance: u32,
    state: ReindeerState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ReindeerState {
    Running(u32),
    Resting(u32),
}

pub fn solution(input: &str) -> (u32, u32) {
    let mut reindeers = input
        .trim()
        .lines()
        .map(|l| Reindeer::parse(l).unwrap())
        .collect::<Vec<_>>();

    let mut leads = vec![0; reindeers.len()];

    for _ in 0..2503 {
        for r in &mut reindeers {
            r.update();
        }

        if let Some(d) = reindeers.iter().map(|r| r.traveled_distance).max() {
            for (i, r) in reindeers.iter().enumerate() {
                if r.traveled_distance == d {
                    leads[i] += 1;
                }
            }
        }
    }

    (
        reindeers.iter().map(|r| r.traveled_distance).max().unwrap(),
        *leads.iter().max().unwrap(),
    )
}

impl<'a> Reindeer<'a> {
    fn update(&mut self) {
        match self.state {
            ReindeerState::Resting(0) => {
                self.state = ReindeerState::Running(self.run_period - 1);
            }
            ReindeerState::Resting(ref mut r) => {
                *r -= 1;
            }
            ReindeerState::Running(0) => {
                self.state = ReindeerState::Resting(self.rest_period - 1);
            }
            ReindeerState::Running(ref mut r) => {
                *r -= 1;
            }
        };

        if let ReindeerState::Running(_) = self.state {
            self.traveled_distance += self.velocity;
        }
    }

    fn parse(input: &'a str) -> Option<Self> {
        let mut parts = input.split_whitespace();
        let name = parts.next()?;

        let mut parts = parts.skip(2);
        let velocity = parts.next()?.parse().ok()?;

        let mut parts = parts.skip(2);
        let run_period = parts.next()?.parse().ok()?;

        let mut parts = parts.skip(6);
        let rest_period = parts.next()?.parse().ok()?;

        Some(Reindeer {
            name,
            velocity,
            rest_period,
            run_period,
            traveled_distance: 0,
            state: ReindeerState::Running(run_period),
        })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn solution() {
        assert_eq!(
            (2655, 1059),
            super::solution(include_str!("../input/day14.txt"))
        );
    }
}

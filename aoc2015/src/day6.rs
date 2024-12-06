use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
struct Instruction {
    command: Command,
    top_left: (usize, usize),
    bottom_right: (usize, usize),
}

#[derive(Debug, PartialEq, Eq)]
enum Command {
    TurnOn,
    TurnOff,
    Toggle,
}

pub fn part1(input: &str) -> usize {
    let mut grid = [[false; 1000]; 1000];

    for i in input.lines().map(|l| l.parse::<Instruction>().unwrap()) {
        for (x, y) in i.points() {
            grid[x][y] = match i.command {
                Command::Toggle => !grid[x][y],
                Command::TurnOff => false,
                Command::TurnOn => true,
            };
        }
    }

    grid.iter().flat_map(|v| v.iter()).filter(|on| **on).count()
}

pub fn part2(input: &str) -> usize {
    let mut grid = vec![vec![0; 1000]; 1000];

    for i in input.lines().map(|l| l.parse::<Instruction>().unwrap()) {
        for (x, y) in i.points() {
            match i.command {
                Command::Toggle => grid[x][y] += 2,
                Command::TurnOff => grid[x][y] = grid[x][y].max(1) - 1,
                Command::TurnOn => grid[x][y] += 1,
            };
        }
    }

    grid.iter().flat_map(|v| v.iter()).sum()
}

impl Instruction {
    fn points(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        (self.top_left.0..=self.bottom_right.0)
            .flat_map(move |x| (self.top_left.1..=self.bottom_right.1).map(move |y| (x, y)))
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(mut input: &str) -> Result<Self, Self::Err> {
        let parse_pair = |l: &str| {
            let mut ps = l.split(',');
            let x = ps.next().ok_or(())?.parse::<usize>().map_err(|_| ())?;
            let y = ps.next().ok_or(())?.parse::<usize>().map_err(|_| ())?;

            Ok((x, y))
        };

        let command = if input.starts_with("turn off") {
            input = input.trim_start_matches("turn off");
            Command::TurnOff
        } else if input.starts_with("turn on") {
            input = input.trim_start_matches("turn on");
            Command::TurnOn
        } else if input.starts_with("toggle") {
            input = input.trim_start_matches("toggle");
            Command::Toggle
        } else {
            return Err(());
        };

        let mut pairs = input.rsplit(' ').take(3);

        let bottom_right = parse_pair(pairs.next().ok_or(())?)?;
        pairs.next().ok_or(())?;
        let top_left = parse_pair(pairs.next().ok_or(())?)?;

        Ok(Instruction {
            command,
            top_left,
            bottom_right,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(400_410, part1(include_str!("../input/day6.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(15_343_601, part2(include_str!("../input/day6.txt")));
    }
}

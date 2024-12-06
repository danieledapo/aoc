#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Forward(i64),
    Up(i64),
    Down(i64),
}

pub fn part1(input: &str) -> i64 {
    let (mut x, mut y) = (0, 0);

    for l in input.lines() {
        let d: Direction = l.parse().unwrap();
        match d {
            Direction::Forward(n) => x += n,
            Direction::Up(n) => y -= n,
            Direction::Down(n) => y += n,
        }
    }

    x * y
}

pub fn part2(input: &str) -> i64 {
    let (mut x, mut y) = (0, 0);
    let mut aim = 0;

    for l in input.lines() {
        let d: Direction = l.parse().unwrap();

        match d {
            Direction::Forward(n) => {
                x += n;
                y += aim * n;
            }
            Direction::Up(n) => aim -= n,
            Direction::Down(n) => aim += n,
        }
    }

    x * y
}

impl std::str::FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (dir, n) = s.split_once(' ').ok_or(())?;
        let n = n.parse().map_err(|_| ())?;

        match dir {
            "forward" => Ok(Direction::Forward(n)),
            "up" => Ok(Direction::Up(n)),
            "down" => Ok(Direction::Down(n)),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(1882980, part1(include_str!("../input/day2.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1971232560, part2(include_str!("../input/day2.txt")));
    }
}

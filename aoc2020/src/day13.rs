use crate::util::lcm;

struct Schedule {
    ts: usize,
    bus: Vec<Option<usize>>,
}

pub fn part1(input: &str) -> usize {
    let schedule = input.parse::<Schedule>().unwrap();

    schedule
        .bus
        .iter()
        .filter_map(|bus| {
            let t = (*bus)?;
            let minutes = (t - (schedule.ts % t)) % t;

            Some((minutes, t))
        })
        .min()
        .map(|(minutes, t)| minutes * t)
        .unwrap()
}

pub fn part2(input: &str) -> usize {
    let schedule = input.parse::<Schedule>().unwrap();

    schedule
        .bus
        .iter()
        .enumerate()
        .fold((1, 0), |(old_period, offset), (n, x)| match x {
            None => (old_period, offset),
            Some(new_period) => (
                lcm(old_period, *new_period),
                (0..)
                    .map(|y| old_period * y + offset)
                    .find(|y| (y + n) % new_period == 0)
                    .unwrap(),
            ),
        })
        .1
}

impl std::str::FromStr for Schedule {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let ts = lines.next().ok_or(())?.parse().map_err(|_| ())?;
        let bus = lines
            .next()
            .ok_or(())?
            .split(',')
            .map(|c| c.parse().ok())
            .collect();

        Ok(Schedule { ts, bus })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(370, part1(include_str!("../input/day13.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(894954360381385, part2(include_str!("../input/day13.txt")));
    }
}

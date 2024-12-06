use std::str::FromStr;

type Point = (i64, i64, i64);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Nanobot {
    o: Point,
    r: u32,
}

pub fn part1(input: &str) -> usize {
    let nanobots = input
        .trim()
        .lines()
        .map(|l| l.parse::<Nanobot>().unwrap())
        .collect::<Vec<_>>();

    let strongest = nanobots.iter().max_by_key(|n| n.r).unwrap();

    nanobots.iter().filter(|n| strongest.in_range(&n.o)).count()
}

///
/// It uses simulated annealing to go from the midpoint of the nanobots towards
/// (0, 0, 0) with a variable speed(using a speed of 1 would probably work as
/// well, but it's too slow). This worked for my input only because the first
/// local maximum it found was the correct one. This was really nice!
///
pub fn part2(input: &str) -> i64 {
    let nanobots = input
        .trim()
        .lines()
        .map(|l| l.parse::<Nanobot>().unwrap())
        .collect::<Vec<_>>();

    let in_range_count = |x, y, z| nanobots.iter().filter(|n| n.in_range(&(x, y, z))).count();

    let mut cur_x = nanobots.iter().map(|n| n.o.0).sum::<i64>() / nanobots.len() as i64;
    let mut cur_y = nanobots.iter().map(|n| n.o.1).sum::<i64>() / nanobots.len() as i64;
    let mut cur_z = nanobots.iter().map(|n| n.o.2).sum::<i64>() / nanobots.len() as i64;
    let mut cur_in_range = in_range_count(cur_x, cur_y, cur_z);

    let max_step = 1 << 31;

    loop {
        let prev_in_range = cur_in_range;
        let mut step = max_step;

        while step > 0 {
            let dirs: [(i64, i64, i64); 7] = [
                (1, 1, 1),
                (1, 1, 0),
                (1, 0, 1),
                (0, 1, 1),
                (1, 0, 0),
                (0, 1, 0),
                (0, 0, 1),
            ];

            for (dx, dy, dz) in &dirs {
                loop {
                    // always go towards (0, 0, 0)
                    let x = cur_x + (if cur_x < 0 { *dx } else { -dx }) * step;
                    let y = cur_y + (if cur_y < 0 { *dy } else { -dy }) * step;
                    let z = cur_z + (if cur_z < 0 { *dz } else { -dz }) * step;

                    let new_in_range = in_range_count(x, y, z);
                    if new_in_range < cur_in_range {
                        break;
                    }

                    cur_x = x;
                    cur_y = y;
                    cur_z = z;
                    cur_in_range = new_in_range;
                }
            }

            step /= 2;
        }

        if prev_in_range == cur_in_range {
            break;
        }
    }

    dist(&(0, 0, 0), &(cur_x, cur_y, cur_z))
}

impl Nanobot {
    fn in_range(&self, p: &Point) -> bool {
        dist(&self.o, p) <= i64::from(self.r)
    }
}

fn dist(p0: &Point, p1: &Point) -> i64 {
    (p0.0 - p1.0).abs() + (p0.1 - p1.1).abs() + (p0.2 - p1.2).abs()
}

impl FromStr for Nanobot {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        fn parse_num<T: FromStr>(o: &str) -> Result<T, ()> {
            o.parse().map_err(|_| ())
        }

        let mut parts = input.split(", ");

        let pos = parts
            .next()
            .ok_or(())?
            .trim_start_matches("pos=<")
            .trim_end_matches('>');

        let mut pos_parts = pos.split(',');
        let x = parse_num(pos_parts.next().ok_or(())?)?;
        let y = parse_num(pos_parts.next().ok_or(())?)?;
        let z = parse_num(pos_parts.next().ok_or(())?)?;

        let r = parse_num(parts.next().ok_or(())?.trim_start_matches("r="))?;

        Ok(Nanobot { r, o: (x, y, z) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(950, part1(include_str!("../input/day23.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(86_871_407, part2(include_str!("../input/day23.txt")));
    }
}

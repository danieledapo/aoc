#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cuboid {
    x0: i64,
    y0: i64,
    z0: i64,
    x1: i64,
    y1: i64,
    z1: i64,
}

pub fn part1(input: &str) -> i64 {
    go(parse(input).filter_map(|(on_off, c)| Some((on_off, c.clamped(50)?))))
}

pub fn part2(input: &str) -> i64 {
    go(parse(input))
}

fn go(input: impl IntoIterator<Item = (bool, Cuboid)>) -> i64 {
    let mut cuboids: Vec<Cuboid> = vec![];

    for (on, cuboid) in input {
        let mut new_cuboids = vec![];

        for c in cuboids {
            // if !c.intersects(&cuboid) {
            //     new_cuboids.push(c);
            //     continue;
            // }

            new_cuboids.extend_from_slice(&c.split(&cuboid));
        }

        if on {
            new_cuboids.push(cuboid);
        }

        cuboids = new_cuboids;
    }

    cuboids.iter().map(Cuboid::volume).sum()
}

impl Cuboid {
    pub fn new((x0, y0, z0): (i64, i64, i64), (x1, y1, z1): (i64, i64, i64)) -> Self {
        Self {
            x0,
            y0,
            z0,
            x1,
            y1,
            z1,
        }
    }

    pub fn intersects(&self, c: &Cuboid) -> bool {
        self.x0 <= c.x1
            && self.y0 <= c.y1
            && self.z0 <= c.z1
            && c.x0 <= self.x1
            && c.y0 <= self.y1
            && c.z0 <= self.z1
    }

    pub fn split(&self, c: &Cuboid) -> Vec<Cuboid> {
        if !self.intersects(c) {
            return vec![self.clone()];
        }

        let mut old = self.clone();

        let mut res = vec![];

        if old.x0 <= c.x1 && old.x1 >= c.x1 {
            res.push(Self::new(
                (c.x1 + 1, old.y0, old.z0),
                (old.x1, old.y1, old.z1),
            ));
            old = Self::new((old.x0, old.y0, old.z0), (c.x1, old.y1, old.z1));
        }

        if old.x0 <= c.x0 && old.x1 >= c.x0 {
            res.push(Self::new(
                (old.x0, old.y0, old.z0),
                (c.x0 - 1, old.y1, old.z1),
            ));
            old = Self::new((c.x0, old.y0, old.z0), (old.x1, old.y1, old.z1));
        }

        if old.y0 <= c.y1 && old.y1 >= c.y1 {
            res.push(Self::new(
                (old.x0, c.y1 + 1, old.z0),
                (old.x1, old.y1, old.z1),
            ));
            old = Self::new((old.x0, old.y0, old.z0), (old.x1, c.y1, old.z1));
        }

        if old.y0 <= c.y0 && old.y1 >= c.y0 {
            res.push(Self::new(
                (old.x0, old.y0, old.z0),
                (old.x1, c.y0 - 1, old.z1),
            ));
            old = Self::new((old.x0, c.y0, old.z0), (old.x1, old.y1, old.z1));
        }

        if old.z0 <= c.z1 && old.z1 >= c.z1 {
            res.push(Self::new(
                (old.x0, old.y0, c.z1 + 1),
                (old.x1, old.y1, old.z1),
            ));
            old = Self::new((old.x0, old.y0, old.z0), (old.x1, old.y1, c.z1));
        }

        if old.z0 <= c.z0 && old.z1 >= c.z0 {
            res.push(Self::new(
                (old.x0, old.y0, old.z0),
                (old.x1, old.y1, c.z0 - 1),
            ));
            // old = Self::new((old.x0, old.y0, c.z0), (old.x1, old.y1, old.z1));
        }

        res
    }

    pub fn clamped(&self, d: i64) -> Option<Self> {
        let c = Cuboid {
            x0: self.x0.max(-d),
            x1: self.x1.min(d),

            y0: self.y0.max(-d),
            y1: self.y1.min(d),

            z0: self.z0.max(-d),
            z1: self.z1.min(d),
        };

        if c.is_normalized() {
            Some(c)
        } else {
            None
        }
    }

    pub fn is_normalized(&self) -> bool {
        self.x0 <= self.x1 && self.y0 <= self.y1 && self.z0 <= self.z1
    }

    pub fn volume(&self) -> i64 {
        (self.x1 - self.x0 + 1) * (self.y1 - self.y0 + 1) * (self.z1 - self.z0 + 1)
    }
}

fn parse(input: &str) -> impl Iterator<Item = (bool, Cuboid)> + '_ {
    let read_range = |l: &str| {
        let mut parts = l.split('=').nth(1).unwrap().split("..");
        let s: i64 = parts.next().unwrap().parse().unwrap();
        let e: i64 = parts.next().unwrap().parse().unwrap();

        (s, e)
    };

    input.lines().map(move |l| {
        let (on, ranges) = l.split_once(' ').unwrap();
        let on = on == "on";

        let mut ranges = ranges.split(',');
        let (x0, x1) = read_range(ranges.next().unwrap());
        let (y0, y1) = read_range(ranges.next().unwrap());
        let (z0, z1) = read_range(ranges.next().unwrap());

        (
            on,
            Cuboid {
                x0,
                x1,
                y0,
                y1,
                z0,
                z1,
            },
        )
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(556501, part1(include_str!("../input/day22.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1217140271559773, part2(include_str!("../input/day22.txt")));
    }
}

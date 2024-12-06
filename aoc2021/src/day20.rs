use std::collections::HashSet;

#[derive(Debug)]
pub struct Image {
    algorithm: Vec<bool>,

    bits: HashSet<(i64, i64)>,
    x0: i64,
    x1: i64,
    y0: i64,
    y1: i64,

    outer: u16,
}

pub fn part1(input: &str) -> usize {
    parse(input).enhance().enhance().bits.len()
}

pub fn part2(input: &str) -> usize {
    let mut img = parse(input);

    for _ in 0..50 {
        img = img.enhance();
    }

    img.bits.len()
}

impl Image {
    pub fn new(algorithm: Vec<bool>) -> Image {
        Image {
            algorithm,
            bits: HashSet::new(),
            x0: i64::max_value(),
            y0: i64::max_value(),

            x1: i64::min_value(),
            y1: i64::min_value(),

            outer: 0,
        }
    }

    pub fn enhance(&self) -> Image {
        let mut res = Image::new(self.algorithm.clone());

        for y in (self.y0 - 1)..=(self.y1 + 1) {
            for x in (self.x0 - 1)..=(self.x1 + 1) {
                let ix = (self.get(x - 1, y - 1) << 8)
                    | (self.get(x, y - 1) << 7)
                    | (self.get(x + 1, y - 1) << 6)
                    | (self.get(x - 1, y) << 5)
                    | (self.get(x, y) << 4)
                    | (self.get(x + 1, y) << 3)
                    | (self.get(x - 1, y + 1) << 2)
                    | (self.get(x, y + 1) << 1)
                    | self.get(x + 1, y + 1);

                if self.algorithm[usize::from(ix)] {
                    res.set(x, y);
                }
            }
        }

        res.outer = self.outer ^ 1;

        res
    }

    pub fn set(&mut self, x: i64, y: i64) {
        self.x0 = self.x0.min(x);
        self.x1 = self.x1.max(x);

        self.y0 = self.y0.min(y);
        self.y1 = self.y1.max(y);

        self.bits.insert((x, y));
    }

    pub fn get(&self, x: i64, y: i64) -> u16 {
        if !(self.x0..=self.x1).contains(&x) || !(self.y0..=self.y1).contains(&y) {
            return self.outer;
        }

        if self.bits.contains(&(x, y)) {
            1
        } else {
            0
        }
    }
}

pub fn parse(input: &str) -> Image {
    let mut lines = input.lines();
    let algorithm = lines.next().unwrap().chars().map(|c| c == '#').collect();

    let mut img = Image::new(algorithm);

    for (l, y) in lines.skip(1).zip(0..) {
        for (v, x) in l.chars().zip(0..) {
            if v != '#' {
                continue;
            }

            img.set(x, y);
        }
    }

    img
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(5647, part1(include_str!("../input/day20.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(15653, part2(include_str!("../input/day20.txt")));
    }
}

use std::collections::HashSet;

pub fn part1(input: &str) -> i32 {
    let (x, y) = PointsIter::new(input).last().unwrap();
    x.abs() + y.abs()
}

pub fn part2(input: &str) -> i32 {
    let mut seen = HashSet::new();
    seen.insert((0, 0));

    for p in PointsIter::new(input) {
        if !seen.insert(p) {
            return p.0.abs() + p.1.abs();
        }
    }

    unreachable!()
}

struct PointsIter<'a> {
    inner: Box<dyn Iterator<Item = &'a str> + 'a>,
    cur_pos: (i32, i32),
    cur_dir: (i32, i32),
    d: i32,
}

impl<'a> PointsIter<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            inner: Box::new(input.trim().split(", ")),
            cur_pos: (0, 0),
            cur_dir: (0, -1),
            d: 0,
        }
    }
}
impl<'a> Iterator for PointsIter<'a> {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.d == 0 {
            let cmd = self.inner.next()?;
            self.cur_dir = match &cmd[..1] {
                "R" => (self.cur_dir.1, -self.cur_dir.0),
                "L" => (-self.cur_dir.1, self.cur_dir.0),
                _ => unreachable!(),
            };
            self.d = cmd[1..].parse().unwrap();
        }

        self.cur_pos.0 += self.cur_dir.0;
        self.cur_pos.1 += self.cur_dir.1;
        self.d -= 1;

        Some(self.cur_pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day1.txt")), 241);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day1.txt")), 116);
    }
}

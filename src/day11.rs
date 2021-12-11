struct Octopuses {
    energies: [u8; 100],
}

pub fn part1(input: &str) -> usize {
    let mut octopuses = parse(input);

    let mut flashes = 0;
    for _ in 0..100 {
        flashes += octopuses.evolve();
    }
    flashes
}

pub fn part2(input: &str) -> u64 {
    let mut octopuses = parse(input);

    for g in 0.. {
        if octopuses.evolve() == 100 {
            return g + 1;
        }
    }

    unreachable!()
}

fn parse(input: &str) -> Octopuses {
    let mut octopuses = Octopuses::new();

    for (r, l) in input.lines().enumerate() {
        for (c, e) in l.chars().enumerate() {
            octopuses.energies[r * 10 + c] = e.to_digit(10).unwrap() as u8;
        }
    }

    octopuses
}

impl Octopuses {
    fn new() -> Self {
        Octopuses {
            energies: [0; 10 * 10],
        }
    }

    fn set(&mut self, r: i32, c: i32, e: u8) {
        assert!((0..10).contains(&r));
        assert!((0..10).contains(&c));
        self.energies[(r * 10 + c) as usize] = e;
    }

    fn evolve(&mut self) -> usize {
        let mut flashes = 0;
        let mut flashed = vec![];

        for (r, c, e) in self.iter_mut() {
            if *e == 9 {
                flashed.push((r, c));
            }

            *e += 1;
        }

        while let Some((r, c)) = flashed.pop() {
            flashes += 1;

            for (r, c, e) in self.neighbors(r, c) {
                if e == 9 {
                    flashed.push((r, c));
                }

                self.set(r, c, e + 1);
            }
        }

        for (_, _, e) in self.iter_mut() {
            if *e > 9 {
                *e = 0;
            }
        }

        flashes
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = (i32, i32, &mut u8)> {
        self.energies
            .iter_mut()
            .enumerate()
            .map(|(i, e)| ((i / 10) as i32, (i % 10) as i32, e))
    }

    fn neighbors(&self, r: i32, c: i32) -> Vec<(i32, i32, u8)> {
        (-1..=1)
            .flat_map(move |dr| {
                (-1..=1).filter_map(move |dc| {
                    if dc == 0 && dr == 0 {
                        return None;
                    }

                    let (r, c) = (r + dr, c + dc);
                    if (0..10).contains(&r) && (0..10).contains(&c) {
                        return Some((r, c, self.energies[(r * 10 + c) as usize]));
                    }

                    None
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(1683, part1(include_str!("../input/day11.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(788, part2(include_str!("../input/day11.txt")));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cell {
    East,
    South,
    Empty,
}

pub fn part1(input: &str) -> u64 {
    let mut world = parse(input);

    for g in 0.. {
        let w = evolve(&world);
        if w == world {
            return g + 1;
        }

        world = w;
    }

    unreachable!()
}

fn evolve(world: &[Vec<Cell>]) -> Vec<Vec<Cell>> {
    let mut new = world.to_owned();

    for (y, l) in world.iter().enumerate() {
        for (x, c) in l.iter().enumerate() {
            match c {
                Cell::East if l[(x + 1) % l.len()] == Cell::Empty => {
                    new[y][(x + 1) % l.len()] = Cell::East;
                    new[y][x] = Cell::Empty;
                }
                _ => {}
            }
        }
    }

    let world = new.clone();

    for (y, l) in world.iter().enumerate() {
        for (x, c) in l.iter().enumerate() {
            match c {
                Cell::South if world[(y + 1) % world.len()][x] == Cell::Empty => {
                    new[(y + 1) % world.len()][x] = Cell::South;
                    new[y][x] = Cell::Empty;
                }
                _ => {}
            }
        }
    }

    new
}

fn parse(input: &str) -> Vec<Vec<Cell>> {
    input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '>' => Cell::East,
                    'v' => Cell::South,
                    '.' => Cell::Empty,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(300, part1(include_str!("../input/day25.txt")));
    }
}

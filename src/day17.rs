use std::ops::RangeInclusive;
use std::str::FromStr;

/// This struct contains only the smallest box of the field to avoid space.
/// Also, this box has been padded at left and right because water could
/// overflow at the sides.
#[derive(Debug, Clone, PartialEq)]
struct Ground {
    grid: Vec<Vec<Cell>>,

    min_y: usize,
    max_y: usize,
    min_x: usize,
    max_x: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Cell {
    Sand,
    Clay,
    RestingWater,
    FlowingWater,
}

pub fn part1(input: &str) -> usize {
    let mut ground: Ground = input.parse().unwrap();

    // problem says to start from 0 but values until min_y are not taken into
    // account
    ground.flood(ground.min_y, 500);
    // println!("{}", ground);

    ground
        .grid
        .iter()
        .flat_map(|row| row.iter())
        .filter(|c| **c == Cell::FlowingWater || **c == Cell::RestingWater)
        .count()
}

pub fn part2(input: &str) -> usize {
    let mut ground: Ground = input.parse().unwrap();

    // problem says to start from 0 but values until min_y are not taken into
    // account
    ground.flood(ground.min_y, 500);
    // println!("{}", ground);

    ground
        .grid
        .iter()
        .flat_map(|row| row.iter())
        .filter(|c| **c == Cell::RestingWater)
        .count()
}

impl Ground {
    fn flood(&mut self, y: usize, x: usize) {
        self.irrigate(y - self.min_y, x - self.min_x + 1);
    }

    fn irrigate(&mut self, starty: usize, startx: usize) -> bool {
        let mut y = self.sprinkle_water(starty, startx);

        if y >= self.grid.len() {
            return true;
        }

        // somebody else already irrigated this region, skip it
        if self.grid[y][startx] == Cell::FlowingWater {
            return true;
        }

        while y != starty {
            y -= 1;

            if self.irrigate_layer(y, startx) {
                return true;
            }
        }

        false
    }

    fn sprinkle_water(&mut self, mut y: usize, startx: usize) -> usize {
        while y < self.grid.len() && self.grid[y][startx] == Cell::Sand {
            self.grid[y][startx] = Cell::FlowingWater;
            y += 1;
        }

        y
    }

    fn irrigate_layer(&mut self, y: usize, x: usize) -> bool {
        let mut left_boundary = x;
        while left_boundary > 0 && self.is_free(y, left_boundary) {
            left_boundary -= 1;
        }

        let mut right_boundary = x;
        while right_boundary + 1 < self.grid[0].len() && self.is_free(y, right_boundary) {
            right_boundary += 1;
        }

        if self.grid[y][left_boundary] == Cell::Clay && self.grid[y][right_boundary] == Cell::Clay {
            for xx in left_boundary + 1..right_boundary {
                self.grid[y][xx] = Cell::RestingWater;
            }
        } else {
            for xx in left_boundary + 1..right_boundary {
                self.grid[y][xx] = Cell::FlowingWater;
            }

            let mut has_overflown = false;

            if self.grid[y + 1][left_boundary] == Cell::Sand {
                has_overflown = self.irrigate(y, left_boundary);
            }

            if self.grid[y + 1][right_boundary] == Cell::Sand {
                has_overflown = self.irrigate(y, right_boundary) || has_overflown;
            }

            if has_overflown {
                return true;
            }
        }

        false
    }

    fn is_free(&self, y: usize, x: usize) -> bool {
        self.grid[y][x] != Cell::Clay && self.grid[y + 1][x] != Cell::Sand
    }
}

impl FromStr for Ground {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let regions = input
            .lines()
            .map(|l| parse_region(l).unwrap())
            .collect::<Vec<_>>();

        let (min_y, min_x, max_y, max_x) = get_bbox(regions);
        let width = max_x - min_x + 1;
        let height = max_y - min_y + 1;

        // add paddings because water could overflow at the sides
        let width = width + 2;

        let mut grid = vec![vec![Cell::Sand; width]; height];

        for c in input.lines() {
            let (ys, xs) = parse_region(c).ok_or(())?;

            for y in ys {
                for x in xs.clone() {
                    grid[y - min_y][x - min_x + 1] = Cell::Clay;
                }
            }
        }

        Ok(Ground {
            grid,
            min_y,
            max_y,
            min_x,
            max_x,
        })
    }
}

impl std::fmt::Display for Ground {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for row in &self.grid {
            writeln!(
                f,
                "{}",
                row.iter()
                    .map(|c| match c {
                        Cell::Sand => ' ',
                        Cell::Clay => '#',
                        Cell::RestingWater => '~',
                        Cell::FlowingWater => '|',
                    })
                    .collect::<String>()
            )?;
        }

        Ok(())
    }
}

fn get_bbox(
    regions: impl IntoIterator<Item = (RangeInclusive<usize>, RangeInclusive<usize>)>,
) -> (usize, usize, usize, usize) {
    regions.into_iter().fold(
        (
            usize::max_value(),
            usize::max_value(),
            usize::min_value(),
            usize::min_value(),
        ),
        |(min_y, min_x, max_y, max_x), (ys, xs)| {
            (
                min_y.min(*ys.start()),
                min_x.min(*xs.start()),
                max_y.max(*ys.end()),
                max_x.max(*xs.end()),
            )
        },
    )
}

fn parse_region(l: &str) -> Option<(RangeInclusive<usize>, RangeInclusive<usize>)> {
    let mut parts = l.split(", ");

    let mut first_part = parts.next()?.split("=");
    let first_coord_id = first_part.next()?;
    let first_n = first_part.next()?.parse().ok()?;

    let mut ys = 0..=0;
    let mut xs = 0..=0;

    if first_coord_id == "x" {
        xs = first_n..=first_n;
    } else {
        ys = first_n..=first_n;
    }

    let mut second_part = parts.next()?.split("=");
    let second_coord_id = second_part.next()?;
    let mut range_part = second_part.next()?.split("..");
    let start = range_part.next()?.parse().ok()?;
    let end = range_part.next()?.parse().ok()?;

    if second_coord_id == "x" {
        xs = start..=end;
    } else {
        ys = start..=end;
    }

    Some((ys, xs))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(28246, part1(include_str!("../input/day17.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(23107, part2(include_str!("../input/day17.txt")));
    }
}

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
    hash::Hash,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Cell {
    Empty,
    Wall,
    Blizzard(Vec<u8>),
}

pub fn part1(input: &str) -> usize {
    walk(input, 0)
}

pub fn part2(input: &str) -> usize {
    walk(input, 2)
}

fn walk(input: &str, stages: usize) -> usize {
    // the blizzards positions cycle, cache them
    let mut states = vec![parse(input)];
    loop {
        let s = blow_wind(states.last().unwrap());
        if &s == states.first().unwrap() {
            break;
        }

        states.push(s);
    }

    let initial_state = &states[0];

    let starty = 0;
    let startx = initial_state[starty]
        .iter()
        .position(|c| *c == Cell::Empty)
        .unwrap();

    let targety = initial_state.len() - 1;
    let targetx = initial_state[targety]
        .iter()
        .position(|c| *c == Cell::Empty)
        .unwrap();

    let mut seen = HashSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), stages, starty, startx));

    while let Some((Reverse(t), mut stage, py, px)) = queue.pop() {
        if stage == 0 && px == targetx && py == targety {
            return t;
        }

        if (stage % 2 == 0 && px == targetx && py == targety)
            || (stage % 2 == 1 && px == startx && py == starty)
        {
            stage -= 1;
        }

        let state_ix = (t + 1) % states.len();
        if !seen.insert((px, py, stage, state_ix)) {
            continue;
        }

        let nt = Reverse(t + 1);
        let new_grid = &states[state_ix];

        if py < new_grid.len() - 1 && new_grid[py + 1][px] == Cell::Empty {
            queue.push((nt, stage, py + 1, px));
        }

        if new_grid[py][px - 1] == Cell::Empty {
            queue.push((nt, stage, py, px - 1));
        }

        if new_grid[py][px + 1] == Cell::Empty {
            queue.push((nt, stage, py, px + 1));
        }

        if py > 0 && new_grid[py - 1][px] == Cell::Empty {
            queue.push((nt, stage, py - 1, px));
        }

        if new_grid[py][px] == Cell::Empty {
            queue.push((nt, stage, py, px));
        }
    }

    unreachable!()
}

fn blow_wind(grid: &Vec<Vec<Cell>>) -> Vec<Vec<Cell>> {
    let (width, height) = (grid[0].len(), grid.len());

    let mut new_grid = vec![vec![Cell::Empty; width]; height];
    for (r, l) in grid.iter().enumerate() {
        for (c, cell) in l.iter().enumerate() {
            match cell {
                Cell::Empty => {}
                Cell::Wall => {
                    new_grid[r][c] = Cell::Wall;
                }
                Cell::Blizzard(bs) => {
                    for &d in bs {
                        let xx;
                        let yy;
                        if d == 0 {
                            yy = r;
                            xx = if c + 1 >= width - 1 { 1 } else { c + 1 };
                        } else if d == 1 {
                            xx = c;
                            yy = if r + 1 >= height - 1 { 1 } else { r + 1 };
                        } else if d == 2 {
                            yy = r;
                            xx = if c <= 1 { width - 2 } else { c - 1 };
                        } else if d == 3 {
                            xx = c;
                            yy = if r <= 1 { height - 2 } else { r - 1 };
                        } else {
                            unreachable!()
                        }

                        match &mut new_grid[yy][xx] {
                            Cell::Wall => unreachable!(),
                            e @ Cell::Empty => *e = Cell::Blizzard(vec![d]),
                            Cell::Blizzard(bs) => bs.push(d),
                        }
                    }
                }
            }
        }
    }

    new_grid
}

fn parse(input: &str) -> Vec<Vec<Cell>> {
    input
        .lines()
        .map(|l| {
            l.chars()
                .map(|char| match char {
                    '.' => Cell::Empty,
                    '#' => Cell::Wall,
                    '>' => Cell::Blizzard(vec![0]),
                    'v' => Cell::Blizzard(vec![1]),
                    '<' => Cell::Blizzard(vec![2]),
                    '^' => Cell::Blizzard(vec![3]),
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
    pub fn test_part1() {
        assert_eq!(
            18,
            part1(
                r#"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"#
            )
        );
        assert_eq!(262, part1(include_str!("../input/day24.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(
            54,
            part2(
                r#"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"#
            )
        );
        assert_eq!(785, part2(include_str!("../input/day24.txt")));
    }
}

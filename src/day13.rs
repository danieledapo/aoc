use crate::day5;

use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Cell {
    Block,
    Empty,
    Wall,
    Paddle,
    Ball,
}

pub fn part1(input: &str) -> usize {
    let mut prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldOutput);

    let mut blocks = HashSet::new();

    loop {
        let x = match prog.run(&mut vec![])[..] {
            [] => break,
            [c] => c,
            _ => unreachable!(),
        };

        let y = prog.run(&mut vec![])[0];
        let tile_id = prog.run(&mut vec![])[0];

        let cell = Cell::from_tile_id(tile_id);
        if cell == Cell::Block {
            blocks.insert((x, y));
        } else {
            blocks.remove(&(x, y));
        }
    }

    blocks.len()
}

pub fn part2(input: &str) -> i64 {
    let mut prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldOutput);
    prog.prog[0] = 2;

    let mut paddle = (0, 0);
    let mut ball = (0, 0);
    let mut joystick = 1;
    let mut score = 0;

    loop {
        let x = match prog.run(&mut vec![joystick])[..] {
            [] => break,
            [c] => c,
            _ => unreachable!(),
        };

        let y = prog.run(&mut vec![joystick])[0];
        let tile_id = prog.run(&mut vec![joystick])[0];

        if x == -1 && y == 0 {
            score = tile_id;
            continue;
        }

        let cell = Cell::from_tile_id(tile_id);
        if cell == Cell::Paddle {
            paddle = (x, y);
        } else if cell == Cell::Ball {
            ball = (x, y);
        }

        joystick = (ball.0 - paddle.0).signum();
    }

    score
}

impl Cell {
    fn from_tile_id(tile_id: i64) -> Self {
        match tile_id {
            0 => Cell::Empty,
            1 => Cell::Wall,
            2 => Cell::Block,
            3 => Cell::Paddle,
            4 => Cell::Ball,
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day13.txt")), 324);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day13.txt")), 15957);
    }
}

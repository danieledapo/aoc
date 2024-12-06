use std::collections::BinaryHeap;

use md5::{Digest, Md5};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    steps: i64,
    position: (i32, i32),
    path: Vec<Direction>,
    doors: u8,
}

pub fn part1(passcode: &str) -> String {
    let (w, h) = (4, 4);

    let mut queue = BinaryHeap::new();
    queue.push(State::new(passcode, 0, (0, 0), vec![]));

    while let Some(state) = queue.pop() {
        if state.position == (w - 1, h - 1) {
            return state.movement_string();
        }

        for d in state.open_doors() {
            let mut new_path = state.path.clone();
            new_path.push(d);

            let new_position = d.apply_to(state.position);
            if !(0..w).contains(&new_position.0) || !(0..h).contains(&new_position.1) {
                continue;
            }

            queue.push(State::new(
                passcode,
                state.steps + 1,
                new_position,
                new_path,
            ));
        }
    }

    unreachable!()
}

pub fn part2(passcode: &str) -> usize {
    let (w, h) = (4, 4);

    let mut queue = BinaryHeap::new();
    queue.push(State::new(passcode, 0, (0, 0), vec![]));

    let mut longest_movement = String::new();

    while let Some(state) = queue.pop() {
        if state.position == (w - 1, h - 1) {
            let movement = state.movement_string();
            if movement.len() > longest_movement.len() {
                longest_movement = movement;
            }

            continue;
        }

        for d in state.open_doors() {
            let mut new_path = state.path.clone();
            new_path.push(d);

            let new_position = d.apply_to(state.position);
            if !(0..w).contains(&new_position.0) || !(0..h).contains(&new_position.1) {
                continue;
            }

            queue.push(State::new(
                passcode,
                state.steps + 1,
                new_position,
                new_path,
            ));
        }
    }

    longest_movement.len()
}

impl State {
    fn new(passcode: &str, steps: i64, position: (i32, i32), path: Vec<Direction>) -> Self {
        let pretty_path = path.iter().map(|d| d.symbol()).collect::<String>();
        let state = format!(
            "{:x}",
            Md5::digest(
                &passcode
                    .as_bytes()
                    .iter()
                    .chain(pretty_path.as_bytes())
                    .copied()
                    .collect::<Vec<_>>()
            )
        );

        let doors = [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
        .iter()
        .zip(state.as_bytes())
        .map(|(d, c)| {
            if (b'b'..=b'f').contains(c) {
                1 << (*d as u8)
            } else {
                0
            }
        })
        .sum();

        Self {
            doors,
            position,
            path,
            steps,
        }
    }

    fn movement_string(&self) -> String {
        self.path.iter().map(|d| d.symbol()).collect()
    }

    fn open_doors(&self) -> impl Iterator<Item = Direction> + '_ {
        [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
        .iter()
        .filter(move |&&d| (self.doors >> d as u8) & 1 == 1)
        .copied()
    }
}

impl Direction {
    fn symbol(self) -> char {
        match self {
            Direction::Up => 'U',
            Direction::Down => 'D',
            Direction::Left => 'L',
            Direction::Right => 'R',
        }
    }

    fn apply_to(self, (x, y): (i32, i32)) -> (i32, i32) {
        match self {
            Direction::Up => (x, y - 1),
            Direction::Down => (x, y + 1),
            Direction::Left => (x - 1, y),
            Direction::Right => (x + 1, y),
        }
    }
}

impl Ord for State {
    fn cmp(&self, o: &State) -> std::cmp::Ordering {
        self.partial_cmp(o).unwrap()
    }
}
impl PartialOrd for State {
    fn partial_cmp(&self, o: &State) -> Option<std::cmp::Ordering> {
        o.steps.partial_cmp(&self.steps)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("ihgpwlah"), "DDRRRD");
        assert_eq!(part1("kglvqrro"), "DDUDRLRRUDRD");
        assert_eq!(part1("ulqzkmiv"), "DRURDRUDDLLDLUURRDULRLDUUDDDRR");
        assert_eq!(part1("veumntbg"), "DDRRULRDRD");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("ihgpwlah"), 370);
        assert_eq!(part2("kglvqrro"), 492);
        assert_eq!(part2("ulqzkmiv"), 830);
        assert_eq!(part2("veumntbg"), 536);
    }
}

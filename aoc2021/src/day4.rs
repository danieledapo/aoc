use std::collections::HashSet;

struct Game {
    to_draw: Vec<u64>,
    boards: Vec<Board>,
}

struct Board {
    cells: Vec<Vec<u64>>,
    marked: HashSet<u64>,
}

pub fn part1(input: &str) -> u64 {
    let mut game = parse(input);

    for n in game.to_draw {
        for board in &mut game.boards {
            board.marked.insert(n);

            if board.has_winner() {
                return board.game_score(n);
            }
        }
    }

    unreachable!()
}

pub fn part2(input: &str) -> u64 {
    let mut game = parse(input);
    let mut last_won_board_score = None;

    for n in game.to_draw {
        for board in &mut game.boards {
            board.marked.insert(n);

            if board.has_winner() {
                last_won_board_score = Some(board.game_score(n));
            }
        }

        game.boards.retain(|b| !b.has_winner());
    }

    last_won_board_score.unwrap()
}

impl Board {
    fn game_score(&self, n: u64) -> u64 {
        n * self
            .cells
            .iter()
            .flat_map(|row| row.iter())
            .filter(|n| !self.marked.contains(*n))
            .sum::<u64>()
    }

    fn has_winner(&self) -> bool {
        for r in &self.cells {
            if r.iter().all(|n| self.marked.contains(n)) {
                return true;
            }
        }

        let w = self.cells[0].len();
        for x in 0..w {
            if (0..w).all(|h| self.marked.contains(&self.cells[h][x])) {
                return true;
            }
        }

        false
    }
}

fn parse(input: &str) -> Game {
    let mut lines = input.lines();

    let to_draw = lines
        .next()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    lines.next().unwrap(); // empty line

    let mut boards = vec![];
    let mut cur_board = Board {
        cells: vec![],
        marked: HashSet::new(),
    };
    for l in lines {
        if l.is_empty() {
            boards.push(cur_board);
            cur_board = Board {
                cells: vec![],
                marked: HashSet::new(),
            };
            continue;
        }

        cur_board
            .cells
            .push(l.split_whitespace().map(|n| n.parse().unwrap()).collect());
    }

    Game { to_draw, boards }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(58838, part1(include_str!("../input/day4.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(6256, part2(include_str!("../input/day4.txt")));
    }
}

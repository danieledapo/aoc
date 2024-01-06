use std::collections::HashMap;

const RIGHT: i64 = 0;
const DOWN: i64 = 1;
const LEFT: i64 = 2;
const UP: i64 = 3;
const DIRS: [(i64, i64); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

#[derive(Debug)]
enum Command {
    Go(i64),
    Turn(bool),
}

#[derive(Debug)]
struct Map {
    area: HashMap<(i64, i64), bool>,
    max: (i64, i64),
    command: Vec<Command>,
}

pub fn part1(input: &str) -> i64 {
    walk(&parse(input), |grid, mut xx, mut yy, dir| {
        let (dx, dy) = DIRS[dir as usize];

        while grid.area.contains_key(&(xx - dx, yy - dy)) {
            xx -= dx;
            yy -= dy;
        }

        (xx, yy, dir)
    })
}

pub fn part2(input: &str) -> i64 {
    // funny...
    walk(&parse(input), |_, mut xx, mut yy, mut dir| {
        if yy < 0 && (50..100).contains(&xx) && dir == UP {
            dir = RIGHT;
            (yy, xx) = (xx + 100, 0);
        } else if xx < 0 && (150..200).contains(&yy) && dir == LEFT {
            dir = DOWN;
            (yy, xx) = (0, yy - 100);
        } else if yy < 0 && (100..150).contains(&xx) && dir == UP {
            (yy, xx) = (199, xx - 100);
        } else if yy >= 200 && (0..50).contains(&xx) && dir == DOWN {
            (yy, xx) = (0, xx + 100);
        } else if xx >= 150 && (0..50).contains(&yy) && dir == RIGHT {
            dir = LEFT;
            (yy, xx) = (149 - yy, 99);
        } else if xx == 100 && (100..150).contains(&yy) && dir == RIGHT {
            dir = LEFT;
            (yy, xx) = (149 - yy, 149);
        } else if yy == 50 && (100..150).contains(&xx) && dir == DOWN {
            dir = LEFT;
            (yy, xx) = (xx - 50, 99);
        } else if xx == 100 && (50..100).contains(&yy) && dir == RIGHT {
            dir = UP;
            (yy, xx) = (49, yy + 50);
        } else if yy == 150 && (50..100).contains(&xx) && dir == DOWN {
            dir = LEFT;
            (yy, xx) = (xx + 100, 49);
        } else if xx == 50 && (150..200).contains(&yy) && dir == RIGHT {
            dir = UP;
            (yy, xx) = (149, yy - 100);
        } else if yy == 99 && (0..50).contains(&xx) && dir == UP {
            dir = RIGHT;
            (yy, xx) = (xx + 50, 50);
        } else if xx == 49 && (50..100).contains(&yy) && dir == LEFT {
            dir = DOWN;
            (yy, xx) = (100, yy - 50);
        } else if xx == 49 && (0..50).contains(&yy) && dir == LEFT {
            dir = RIGHT;
            (yy, xx) = (149 - yy, 0);
        } else if xx < 0 && (100..150).contains(&yy) && dir == LEFT {
            dir = RIGHT;
            (yy, xx) = (149 - yy, 50);
        }

        (xx, yy, dir)
    })
}

fn walk(grid: &Map, wrap_around: impl Fn(&Map, i64, i64, i64) -> (i64, i64, i64)) -> i64 {
    let mut dir = 0_i64;
    let (mut x, mut y) = (0, 0);
    while *grid.area.get(&(x, y)).unwrap_or(&true) {
        x += 1;
    }

    for c in &grid.command {
        match *c {
            Command::Turn(d) => dir = (dir + if d { 1 } else { -1 }).rem_euclid(4),
            Command::Go(n) => {
                for _ in 0..n {
                    let (dx, dy) = DIRS[dir as usize];

                    let mut xx = x + dx;
                    let mut yy = y + dy;
                    let mut ddir = dir;

                    if !grid.area.contains_key(&(xx, yy)) {
                        // wrap-around
                        (xx, yy, ddir) = wrap_around(grid, xx, yy, ddir);
                    }

                    if grid.area[&(xx, yy)] {
                        // wall
                        break;
                    }

                    // open
                    x = xx;
                    y = yy;
                    dir = ddir;
                }
            }
        }
    }

    (x + 1) * 4 + (y + 1) * 1000 + dir
}

fn parse(input: &str) -> Map {
    let mut m = Map {
        area: HashMap::new(),
        max: (-1, -1),
        command: vec![],
    };

    let mut empty = false;
    for (row, l) in input.lines().enumerate() {
        if l.is_empty() {
            empty = true;
            continue;
        }

        if empty {
            let mut chars = l.chars().peekable();
            while let Some(c) = chars.next() {
                if let Some(mut n) = c.to_digit(10) {
                    while let Some(nn) = chars.peek().and_then(|c| c.to_digit(10)) {
                        n = n * 10 + nn;
                        chars.next().unwrap();
                    }

                    m.command.push(Command::Go(n.into()));
                    continue;
                }

                m.command.push(Command::Turn(c == 'R'));
            }
            continue;
        }

        for (col, c) in l.chars().enumerate() {
            if c == ' ' {
                continue;
            }
            let (x, y) = (col as i64, row as i64);

            m.max = (m.max.0.max(x), m.max.1.max(y));

            m.area.insert((x, y), c == '#');
        }
    }

    m
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(190066, part1(include_str!("../input/day22.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(134170, part2(include_str!("../input/day22.txt")));
    }
}

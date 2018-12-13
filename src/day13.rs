use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Cart {
    direction: u8,
    turn_times: u8,
    position: (usize, usize),
}

pub fn part1(input: &str) -> (usize, usize) {
    let (trails, mut carts) = parse_input(input);

    loop {
        carts.sort_by_key(|c| (c.position.1, c.position.0));
        let mut carts_positions = carts.iter().map(|c| c.position).collect::<HashSet<_>>();

        for cart in carts.iter_mut() {
            carts_positions.remove(&cart.position);

            cart.step();

            if !carts_positions.insert(cart.position) {
                return cart.position;
            }

            match trails.get(&cart.position).unwrap_or(&b'|') {
                b'/' => cart.slash_turn(),
                b'\\' => cart.backslash_turn(),
                b'+' => cart.intersection_turn(),
                _ => { /* ignore spaces and straight lines */ }
            };
        }
    }
}

pub fn part2(input: &str) -> (usize, usize) {
    let (trails, mut carts) = parse_input(input);

    while carts.len() > 1 {
        carts.sort_by_key(|c| (c.position.1, c.position.0));

        let mut carts_positions = carts.iter().map(|c| c.position).collect::<HashSet<_>>();
        let mut new_carts = vec![];

        for mut cart in carts {
            carts_positions.remove(&cart.position);

            cart.step();

            if !carts_positions.contains(&cart.position) {
                carts_positions.insert(cart.position);

                match trails.get(&cart.position).unwrap_or(&b'|') {
                    b'/' => cart.slash_turn(),
                    b'\\' => cart.backslash_turn(),
                    b'+' => cart.intersection_turn(),
                    _ => { /* ignore spaces and straight lines */ }
                };

                new_carts.push(cart);
            } else {
                new_carts.retain(|c| c.position != cart.position);
            }
        }

        carts = new_carts;
    }

    carts[0].position
}

impl Cart {
    fn step(&mut self) {
        match self.direction {
            b'^' => self.position.1 -= 1,
            b'v' => self.position.1 += 1,
            b'<' => self.position.0 -= 1,
            b'>' => self.position.0 += 1,
            _ => unimplemented!(),
        };
    }

    fn intersection_turn(&mut self) {
        match self.turn_times {
            0 => self.turn_left(),
            1 => { /* straight line */ }
            2 => self.turn_right(),
            _ => unimplemented!(),
        };

        self.turn_times = (self.turn_times + 1) % 3;
    }

    fn slash_turn(&mut self) {
        match self.direction {
            b'^' => self.direction = b'>',
            b'<' => self.direction = b'v',
            b'v' => self.direction = b'<',
            b'>' => self.direction = b'^',
            _ => unimplemented!(),
        }
    }

    fn backslash_turn(&mut self) {
        match self.direction {
            b'^' => self.direction = b'<',
            b'>' => self.direction = b'v',
            b'v' => self.direction = b'>',
            b'<' => self.direction = b'^',
            _ => unimplemented!(),
        }
    }

    fn turn_left(&mut self) {
        self.direction = match self.direction {
            b'^' => b'<',
            b'<' => b'v',
            b'v' => b'>',
            b'>' => b'^',
            _ => unimplemented!(),
        };
    }

    fn turn_right(&mut self) {
        self.direction = match self.direction {
            b'^' => b'>',
            b'>' => b'v',
            b'v' => b'<',
            b'<' => b'^',
            _ => unimplemented!(),
        };
    }
}

fn parse_input(input: &str) -> (HashMap<(usize, usize), u8>, Vec<Cart>) {
    let mut trails = HashMap::new();
    let mut carts = vec![];

    for (y, l) in input.lines().enumerate() {
        for (x, c) in l.bytes().enumerate() {
            if c == b' ' {
                continue;
            }

            if is_cart(c) {
                carts.push(Cart {
                    position: (x, y),
                    turn_times: 0,
                    direction: c,
                });
            } else {
                trails.insert((x, y), c);
            }
        }
    }

    (trails, carts)
}

fn is_cart(c: u8) -> bool {
    [b'>', b'<', b'^', b'v'].contains(&c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!((26, 99), part1(include_str!("../input/day13.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!((62, 48), part2(include_str!("../input/day13.txt")));
    }
}

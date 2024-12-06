#[derive(Debug, Clone, PartialEq, Eq)]
enum Op {
    SwapPos(usize, usize),
    SwapChar(u8, u8),
    RotateLeft(usize),
    RotateRight(usize),
    RotateOnLetter(u8, bool),
    Reverse(usize, usize),
    Move(usize, usize),
}

pub fn part1(input: &str) -> String {
    let rules = parse(input);

    let mut s = b"abcdefgh".to_vec();
    for r in rules {
        r.exec(&mut s);
    }

    String::from_utf8(s).unwrap()
}

pub fn part2(input: &str) -> String {
    let mut rules = parse(input);
    rules.reverse();
    for r in &mut rules {
        *r = r.undo();
    }

    let mut s = b"fbgdceah".to_vec();
    for r in rules {
        r.exec(&mut s);
    }

    String::from_utf8(s).unwrap()
}

impl Op {
    pub fn exec(&self, s: &mut [u8]) {
        use Op::*;

        match *self {
            SwapPos(i, j) => s.swap(i, j),
            SwapChar(x, y) => {
                for c in s {
                    if *c == x {
                        *c = y;
                    } else if *c == y {
                        *c = x;
                    }
                }
            }
            RotateRight(steps) => s.rotate_right(steps),
            RotateLeft(steps) => s.rotate_left(steps),
            RotateOnLetter(c, false) => {
                let i = s.iter().position(|cc| c == *cc).unwrap();
                let n = (1 + i + (i / 4).min(1)) % s.len();
                s.rotate_right(n);
            }
            RotateOnLetter(c, true) => {
                // there's probably an analytical formula to find out n, but I couldn't find it.
                // Bruteforce all possible rotations...
                let n = (1..=s.len())
                    .find(|steps| {
                        let mut ss = s.to_vec();
                        ss.rotate_left(*steps);
                        RotateOnLetter(c, false).exec(&mut ss);

                        ss == s
                    })
                    .unwrap();
                s.rotate_left(n);
            }
            Reverse(i, j) => s[i..=j].reverse(),
            Move(i, j) => {
                if i < j {
                    for z in i..j {
                        s.swap(z, z + 1);
                    }
                } else {
                    for z in (j..i).rev() {
                        s.swap(z, z + 1);
                    }
                }
            }
        }
    }

    pub fn undo(&self) -> Op {
        use Op::*;

        match *self {
            SwapPos(i, j) => SwapPos(i, j),
            SwapChar(x, y) => SwapChar(x, y),
            Reverse(i, j) => Reverse(i, j),

            RotateRight(steps) => RotateLeft(steps),
            RotateLeft(steps) => RotateRight(steps),

            Move(i, j) => Move(j, i),

            RotateOnLetter(c, inverse) => RotateOnLetter(c, !inverse),
        }
    }
}

fn parse(input: &str) -> Vec<Op> {
    input
        .lines()
        .map(|l| {
            if l.starts_with("swap position ") {
                let mut parts = l.trim_start_matches("swap position ").split_whitespace();
                let x = parts.next().unwrap().parse().unwrap();
                let y = parts.last().unwrap().parse().unwrap();
                return Op::SwapPos(x, y);
            }

            if l.starts_with("swap letter ") {
                let mut parts = l.trim_start_matches("swap letter ").split_whitespace();
                let x = parts.next().unwrap().chars().next().unwrap();
                let y = parts.last().unwrap().chars().next().unwrap();
                return Op::SwapChar(x as u8, y as u8);
            }

            if l.starts_with("rotate based on position of letter ") {
                return Op::RotateOnLetter(
                    l.trim_start_matches("rotate based on position of letter ")
                        .bytes()
                        .next()
                        .unwrap(),
                    false,
                );
            }

            if l.starts_with("rotate ") {
                let mut parts = l.trim_start_matches("rotate ").split_whitespace();
                let d = parts.next().unwrap();
                let steps = parts.next().unwrap().parse().unwrap();
                return match d {
                    "left" => Op::RotateLeft(steps),
                    "right" => Op::RotateRight(steps),
                    _ => unreachable!(),
                };
            }

            if l.starts_with("reverse positions ") {
                let mut parts = l
                    .trim_start_matches("reverse positions ")
                    .split_whitespace();
                let x = parts.next().unwrap().parse().unwrap();
                let y = parts.last().unwrap().parse().unwrap();
                return Op::Reverse(x, y);
            }

            if l.starts_with("move position ") {
                let mut parts = l.trim_start_matches("move position ").split_whitespace();
                let x = parts.next().unwrap().parse().unwrap();
                let y = parts.last().unwrap().parse().unwrap();
                return Op::Move(x, y);
            }

            unreachable!()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day21.txt")), "gfdhebac");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day21.txt")), "dhaegfbc");
    }
}

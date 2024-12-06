#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    DealNewStack,
    Cut(i128),
    Deal(i128),
}

pub fn part1(input: &str) -> i128 {
    let deck_size = 10_007;
    let to_find = 2019;

    let mut pos = to_find;

    for cmd in parse(input) {
        match cmd {
            Command::DealNewStack => {
                pos = deck_size - 1 - pos;
            }
            Command::Cut(c) => {
                pos = (pos - c + deck_size) % deck_size;
            }
            Command::Deal(inc) => {
                pos = inc * pos % deck_size;
            }
        }
    }

    pos
}

pub fn part2(input: &str) -> i128 {
    let deck_size: i128 = 119_315_717_514_047;
    let iterations: i128 = 101_741_582_076_661;
    let number_at: i128 = 2020;
    let input = parse(input).collect::<Vec<_>>();

    let mut a = 1;
    let mut b = 0;

    for op in input.iter().rev() {
        match op {
            Command::Cut(n) => {
                b += if *n < 0 { n + deck_size } else { *n };
            }
            Command::Deal(n) => {
                let inv = modinv(*n, deck_size);
                a = a * inv % deck_size;
                b = b * inv % deck_size;
            }
            Command::DealNewStack => {
                b += 1;
                b *= -1;
                a *= -1;
            }
        }

        a %= deck_size;
        b %= deck_size;

        if a < 0 {
            a += deck_size;
        }

        if b < 0 {
            b += deck_size;
        }
    }

    let i1 = modp(a, iterations, deck_size) * number_at % deck_size;
    let i2 = (modp(a, iterations, deck_size) + deck_size - 1) % deck_size;
    let i3 = b * i2 % deck_size;
    let i4 = modp(a - 1, deck_size - 2, deck_size);

    (i1 + i3 * i4) % deck_size
}

fn modinv(mut a: i128, mut base: i128) -> i128 {
    if base == 1 {
        return 0;
    }

    let orig = base;

    let mut x = 1;
    let mut y = 0;

    while a > 1 {
        let q = a / base;
        let tmp = base;
        base = a % base;
        a = tmp;
        let tmp = y;
        y = x - q * y;
        x = tmp;
    }

    if x < 0 {
        x + orig
    } else {
        x
    }
}

fn modp(b: i128, exp: i128, base: i128) -> i128 {
    let mut x = 1;
    let mut p = b % base;

    for i in 0..128 {
        if 1 & (exp >> i) == 1 {
            x = x * p % base;
        }

        p = p * p % base;
    }

    x
}
fn parse(input: &str) -> impl Iterator<Item = Command> + '_ {
    input.lines().map(|l| {
        if l == "deal into new stack" {
            return Command::DealNewStack;
        }

        if l.starts_with("deal") {
            Command::Deal(l.split_whitespace().last().unwrap().parse().unwrap())
        } else if l.starts_with("cut") {
            Command::Cut(l.split_whitespace().last().unwrap().parse().unwrap())
        } else {
            unreachable!()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day22.txt")), 3074);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day22.txt")), 104073967000066);
    }
}

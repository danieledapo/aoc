use std::collections::HashMap;

use itertools::iproduct;

type Number = HashMap<u32, u32>;

pub fn part1(input: &str) -> u32 {
    let nums = parse(input);

    let mut s = nums[0].clone();
    for a in nums.iter().skip(1) {
        s = add(&s, &a);
    }

    magnitude(&s)
}

pub fn part2(input: &str) -> u32 {
    let nums = parse(input);

    iproduct!(nums.iter(), nums.iter())
        .filter(|(a, b)| a != b)
        .map(|(a, b)| magnitude(&add(&a, &b)))
        .max()
        .unwrap()
}

fn magnitude(a: &Number) -> u32 {
    a.iter()
        .map(|(k, v)| {
            v * format!("{:b}", k)
                .chars()
                .skip(1)
                .map(|c| 3 - if c == '1' { 1 } else { 0 })
                .product::<u32>()
        })
        .sum()
}

fn add(a: &Number, b: &Number) -> Number {
    let mut c = Number::new();

    for (k, v) in a {
        c.insert(key(*k, 0b10), *v);
    }

    for (k, v) in b {
        c.insert(key(*k, 0b11), *v);
    }

    while reduce(&mut c) {
        // keep reducing
    }

    c
}

fn reduce(n: &mut Number) -> bool {
    let mut keys: Vec<_> = n.keys().copied().collect();
    keys.sort_unstable_by_key(|n| format!("{:b}", n));

    // Explode
    for (i, &k1) in keys.iter().enumerate() {
        if k1 < 32 {
            continue;
        }

        let k2 = keys[i + 1];

        if i != 0 {
            let dv = n[&k1];
            n.entry(keys[i - 1]).and_modify(|v| *v += dv);
        }

        if i != keys.len() - 2 {
            let dv = n[&k2];
            n.entry(keys[i + 2]).and_modify(|v| *v += dv);
        }

        n.remove(&k1);
        n.remove(&k2);
        n.insert(k1 / 2, 0);

        return true;
    }

    // Split
    for k in keys {
        let v = n[&k];
        if v < 10 {
            continue;
        }

        n.insert(2 * k, v / 2);
        n.insert(2 * k + 1, v - v / 2);
        n.remove(&k);

        return true;
    }

    false
}

fn key(index: u32, mut prefix: u32) -> u32 {
    let s = format!("{:b}", index);
    for c in s.chars().skip(1) {
        prefix = (prefix << 1) | (if c == '1' { 1 } else { 0 });
    }
    prefix
}

fn parse(input: &str) -> Vec<Number> {
    input
        .lines()
        .map(|l| {
            let mut n = Number::new();
            parse_number(&mut l.chars(), &mut n, 1);
            n
        })
        .collect()
}

fn parse_number(chars: &mut impl Iterator<Item = char>, n: &mut Number, index: u32) {
    let c = chars.next().unwrap();

    if c == '[' {
        parse_number(chars, n, 2 * index);
        chars.next().unwrap(); // ,
        parse_number(chars, n, 2 * index + 1);
        chars.next().unwrap(); // ]

        return;
    }

    if let Some(d) = c.to_digit(10) {
        n.insert(index, d);
        return;
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(4207, part1(include_str!("../input/day18.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(4635, part2(include_str!("../input/day18.txt")));
    }
}

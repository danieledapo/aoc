use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> i32 {
    let happinesses = input
        .trim()
        .lines()
        .map(|l| {
            let (src, dst, dist) = parse_line(l).unwrap();

            ((src, dst), dist)
        })
        .collect::<HashMap<_, _>>();

    best_happiness(&happinesses)
}

pub fn part2(input: &str) -> i32 {
    let mut happinesses = input
        .trim()
        .lines()
        .map(|l| {
            let (src, dst, dist) = parse_line(l).unwrap();

            ((src, dst), dist)
        })
        .collect::<HashMap<_, _>>();

    let people = happinesses
        .keys()
        .map(|(src, _)| *src)
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();

    for p in people {
        happinesses.insert(("d", p), 0);
        happinesses.insert((p, "d"), 0);
    }

    best_happiness(&happinesses)
}

fn best_happiness<'a>(happinesses: &HashMap<(&'a str, &'a str), i32>) -> i32 {
    let people = happinesses
        .keys()
        .map(|(src, _)| *src)
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();

    // it's inefficient, but it's reasonably fast
    let tables = permutations(&people);

    let table_happiness = tables.into_iter().map(|p| {
        (0..p.len())
            .map(|i| {
                let next = (i + 1) % p.len();
                let prev = (i + p.len() - 1) % p.len();

                happinesses[&(p[i], p[next])] + happinesses[&(p[i], p[prev])]
            })
            .sum()
    });

    table_happiness.max().unwrap()
}

pub fn permutations<'a>(v: &[&'a str]) -> Vec<Vec<&'a str>> {
    if v.is_empty() {
        return vec![];
    }

    if v.len() == 1 {
        return vec![vec![v[0]]];
    }

    if v.len() == 2 {
        return vec![vec![v[0], v[1]], vec![v[1], v[0]]];
    }

    (0..v.len())
        .flat_map(|n| {
            let v = v
                .iter()
                .cloned()
                .cycle()
                .skip(n)
                .take(v.len())
                .collect::<Vec<&'a str>>();

            permutations(&v[1..]).into_iter().map(move |mut p| {
                p.push(v[0]);
                p
            })
        })
        .collect()
}

fn parse_line(l: &str) -> Option<(&str, &str, i32)> {
    let mut parts = l.trim_end_matches('.').split_whitespace();

    let src = parts.next()?;
    let mut parts = parts.skip(1);

    let sign = parts.next()?;
    let happiness = parts.next()?.parse::<i32>().ok()?;
    let happiness = if sign == "lose" {
        -happiness
    } else {
        happiness
    };

    let mut parts = parts.skip(6);
    let dst = parts.next()?;

    Some((src, dst, happiness))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(733, part1(include_str!("../input/day13.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(725, part2(include_str!("../input/day13.txt")));
    }
}

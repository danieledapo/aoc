use std::collections::{HashMap, HashSet};

pub fn solution(input: &str) -> (u32, u32) {
    let dists = input
        .trim()
        .lines()
        .flat_map(|l| {
            let (src, dst, dist) = parse_line(l).unwrap();

            std::iter::once(((src, dst), dist)).chain(std::iter::once(((dst, src), dist)))
        })
        .collect::<HashMap<_, _>>();

    let cities = dists
        .keys()
        .flat_map(|(src, dst)| std::iter::once(*src).chain(std::iter::once(*dst)))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();

    let paths = permutations(&cities);

    let paths_distances = paths.into_iter().map(|p| {
        (0..p.len() - 1)
            .map(|i| {
                dists
                    .get(&(p[i], p[i + 1]))
                    .or_else(|| dists.get(&(p[i + 1], p[i])))
                    .unwrap()
            })
            .sum()
    });

    paths_distances.fold((u32::max_value(), 0), |(min, max), d| {
        (min.min(d), max.max(d))
    })
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

fn parse_line(l: &str) -> Option<(&str, &str, u32)> {
    let mut parts = l.split_whitespace();

    let src = parts.next()?;
    parts.next()?; // to
    let dst = parts.next()?;
    parts.next()?; // =

    let dist = parts.next()?.parse::<u32>().ok()?;

    Some((src, dst, dist))
}

#[cfg(test)]
mod tests {
    #[test]
    fn solution() {
        assert_eq!(
            (207, 804),
            super::solution(include_str!("../input/day9.txt"))
        );
    }
}

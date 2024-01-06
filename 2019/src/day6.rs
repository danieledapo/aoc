use std::collections::HashMap;

pub fn part1(input: &str) -> usize {
    let orbits = parse_orbits(input);

    let mut cache = HashMap::new();

    orbits
        .keys()
        .map(|o| find_connected_orbits(&orbits, *o, &mut cache))
        .sum()
}

pub fn part2(input: &str) -> usize {
    let orbits = parse_orbits(input);

    let san_orbits = get_parent_orbits(&orbits, "SAN");

    let mut steps = 0;
    let mut cur_orbit = orbits["YOU"];
    loop {
        if let Some(t) = san_orbits.get(&cur_orbit) {
            break steps + t;
        }

        steps += 1;
        cur_orbit = orbits[cur_orbit];
    }
}

fn parse_orbits(input: &str) -> HashMap<&str, &str> {
    let mut graph = HashMap::new();

    for l in input.lines() {
        let mut parts = l.split(')');
        let parent = parts.next().unwrap();
        let child = parts.next().unwrap();

        graph.insert(child, parent);
    }

    graph
}

fn find_connected_orbits<'a>(
    orbits: &HashMap<&'a str, &'a str>,
    n: &'a str,
    cache: &mut HashMap<&'a str, usize>,
) -> usize {
    if let Some(o) = cache.get(n) {
        return *o;
    }

    let ss = orbits
        .get(n)
        .map(|p| 1 + find_connected_orbits(orbits, p, cache))
        .unwrap_or(0);

    cache.insert(n, ss);
    ss
}

fn get_parent_orbits<'a>(
    orbits: &HashMap<&'a str, &'a str>,
    mut n: &'a str,
) -> HashMap<&'a str, usize> {
    let mut parents = HashMap::new();
    let mut oi = 0_usize;

    while let Some(p) = orbits.get(n) {
        parents.insert(*p, oi);
        oi += 1;
        n = *p;
    }

    parents
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day6.txt")), 295936);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day6.txt")), 457);
    }
}

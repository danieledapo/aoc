use std::collections::{BTreeMap, BTreeSet};

pub fn part1(input: &str) -> String {
    visit(input, 1, 0).1
}

pub fn part2(input: &str, workers: usize, delay: u64) -> u64 {
    visit(input, workers, delay).0
}

pub fn visit(input: &str, workers: usize, delay: u64) -> (u64, String) {
    let mut deps = parse_deps(input);

    let mut to_visit = BTreeSet::new();
    let mut workers = vec![None; workers];
    let mut time = 0;
    let mut path = String::new();

    while !deps.is_empty() || !to_visit.is_empty() || !workers.is_empty() {
        let leaves = deps
            .iter()
            .filter(|&(_, d)| d.is_empty())
            .map(|(c, _)| *c)
            .collect::<BTreeSet<_>>();

        for l in &leaves {
            deps.remove(l);
        }

        to_visit.extend(leaves.into_iter());

        for w in &mut workers {
            if w.is_none() {
                if let Some(l) = smallest(&to_visit) {
                    *w = Some((l, u64::from(l as u8 - b'A') + 1 + delay));
                    to_visit.remove(&l);
                }
            }
        }

        let smallest = workers.iter().filter_map(|&w| w).min_by_key(|&(_, t)| t);
        let (c, smallest_t) = match smallest {
            None => break,
            Some(t) => t,
        };

        time += smallest_t;
        path.push(c);

        for w in &mut workers {
            if let Some((l, t)) = w {
                if *t == smallest_t {
                    for ds in deps.values_mut() {
                        ds.remove(&l);
                    }

                    *w = None;
                } else {
                    *t -= smallest_t;
                }
            }
        }
    }

    (time, path)
}

fn smallest(tree: &BTreeSet<char>) -> Option<char> {
    tree.iter().cloned().next()
}

fn parse_deps(input: &str) -> BTreeMap<char, BTreeSet<char>> {
    let mut deps: BTreeMap<char, BTreeSet<char>> = BTreeMap::new();

    for line in input.lines() {
        let mut parts = line.split_whitespace();
        let step = parts.nth(1).unwrap().chars().next().unwrap();
        let before = parts.nth(5).unwrap().chars().next().unwrap();

        deps.entry(before).or_default().insert(step);
        deps.entry(step).or_default();
    }

    deps
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(
            "CABDFE",
            part1(
                r"Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.",
            )
        );
        assert_eq!(
            "JKNSTHCBGRVDXWAYFOQLMPZIUE",
            part1(include_str!("../input/day7.txt"))
        );
    }

    #[test]
    fn solution_part2() {
        assert_eq!(
            15,
            part2(
                r"Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.",
                2,
                0
            )
        );
        assert_eq!(755, part2(include_str!("../input/day7.txt"), 5, 60));
    }
}

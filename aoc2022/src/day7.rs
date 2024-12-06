use std::{collections::HashMap, path::PathBuf};

pub type Tree = HashMap<PathBuf, Vec<(String, i64)>>;

pub fn part1(input: &str) -> i64 {
    let mut sizes = HashMap::new();
    fill_dir_sizes(&"/".into(), &parse(input), &mut sizes);
    sizes.values().filter(|c| **c < 100_000).copied().sum()
}

pub fn part2(input: &str) -> i64 {
    let mut sizes = HashMap::new();
    fill_dir_sizes(&"/".into(), &parse(input), &mut sizes);

    let to_free = 30_000_000 - (70_000_000 - sizes[&PathBuf::from("/")]);

    sizes
        .values()
        .filter(|c| **c > to_free)
        .copied()
        .min()
        .unwrap()
}

fn fill_dir_sizes(n: &PathBuf, t: &Tree, sizes: &mut HashMap<PathBuf, i64>) {
    if sizes.contains_key(n) {
        return;
    }

    let c = t[n]
        .iter()
        .map(|(nc, c)| {
            if *c == 0 {
                let mut ncc = n.clone();
                ncc.push(nc);

                fill_dir_sizes(&ncc, t, sizes);
                sizes[&ncc]
            } else {
                *c
            }
        })
        .sum();
    sizes.insert(n.clone(), c);
}

fn parse(input: &str) -> Tree {
    let mut tree = Tree::new();

    let mut curr = PathBuf::new();
    let mut lines = input.lines().peekable();
    while let Some(l) = lines.next() {
        if l == "$ cd .." {
            curr.pop();
            continue;
        }

        if l == "$ cd /" {
            curr = "/".into();
            continue;
        }

        if l.starts_with("$ cd") {
            curr.push(l.split_whitespace().nth(2).unwrap());
            continue;
        }

        if l.starts_with("$ ls") {
            while let Some(l) = lines.peek() {
                if l.starts_with('$') {
                    break;
                }

                let e = lines.next().unwrap();
                let (a, b) = e.split_once(' ').unwrap();

                tree.entry(curr.clone()).or_default().push((
                    b.to_string(),
                    if a == "dir" { 0 } else { a.parse().unwrap() },
                ));
            }
        }
    }

    tree
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(1206825, part1(include_str!("../input/day7.txt"),));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(9608311, part2(include_str!("../input/day7.txt"),));
    }
}

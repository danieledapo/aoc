use std::collections::HashMap;

type Graph<'a> = HashMap<&'a str, Vec<&'a str>>;

pub fn part1(input: &str) -> usize {
    let graph = parse(input);
    find_paths(&graph, 1).len()
}

pub fn part2(input: &str) -> usize {
    let graph = parse(input);
    find_paths(&graph, 2).len()
}

fn find_paths<'a>(graph: &'a Graph, at_most: usize) -> Vec<Vec<&'a str>> {
    let mut paths = vec![];
    let mut stack = vec![(vec!["start"], {
        let mut h = HashMap::new();
        h.insert("start", 1);
        h
    })];

    while let Some((path, counts)) = stack.pop() {
        let cur_position = path.last().unwrap();
        if *cur_position == "end" {
            paths.push(path);
            continue;
        }

        let positions = match graph.get(cur_position) {
            None => continue,
            Some(positions) => positions,
        };

        for position in positions {
            let mut new_counts = counts.clone();

            if position.chars().next().unwrap().is_lowercase() {
                let c = new_counts.entry(position).or_insert(0);
                *c += 1;
                if *c > at_most {
                    continue;
                }

                if at_most > 1 && new_counts.values().filter(|c| **c == at_most).count() > 1 {
                    continue;
                }
            }

            let mut new_path = path.clone();
            new_path.push(*position);

            stack.push((new_path, new_counts));
        }
    }

    paths
}

fn parse(input: &str) -> Graph {
    let mut graph = HashMap::new();
    for l in input.lines() {
        let (s, d) = l.split_once("-").unwrap();

        if s != "end" && d != "start" {
            graph.entry(s).or_insert_with(Vec::new).push(d);
        }

        if s != "start" && d != "end" {
            graph.entry(d).or_insert_with(Vec::new).push(s);
        }
    }

    graph
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(3450, part1(include_str!("../input/day12.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(96528, part2(include_str!("../input/day12.txt")));
    }
}

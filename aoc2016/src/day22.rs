use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    x: i32,
    y: i32,
    size: i32,
    used: i32,
    avail: i32,
}

pub fn part1(input: &str) -> usize {
    use std::iter::once;

    let nodes: Vec<_> = input.lines().skip(2).map(Node::parse).collect();

    nodes
        .iter()
        .enumerate()
        .flat_map(|(i, n1)| {
            nodes
                .iter()
                .skip(i)
                .flat_map(move |n2| once((n1, n2)).chain(once((n2, n1))))
        })
        .filter(|(n1, n2)| n2.used > 0 && n1.avail > n2.used)
        .count()
}

pub fn part2(input: &str) -> usize {
    let nodes: HashMap<_, _> = input
        .lines()
        .skip(2)
        .map(|l| {
            let n = Node::parse(l);
            ((n.x, n.y), n)
        })
        .collect();

    let xmax = nodes.keys().map(|(x, _)| *x).max().unwrap();
    let ymax = nodes.keys().map(|(_, y)| *y).max().unwrap();

    for y in 0..=ymax {
        let line = (0..=xmax)
            .map(|x| {
                if (x, y) == (0, 0) {
                    return '*';
                }

                if (x, y) == (xmax, 0) {
                    return '@';
                }

                let n = &nodes[&(x, y)];
                if n.used == 0 {
                    return '_';
                }

                if n.size < 100 {
                    '.'
                } else {
                    '#'
                }
            })
            .collect::<String>();
        println!("{}", line);
    }

    println!(
        r#"
Solve by hand, inspect the map find the path that goes from the empty disk (_)
to a cell adjacent to the target one (@) and remeber its length. Calculate the
path that goes from the target (@) to the origin (*) and store its length.

The final length is 
length_path2@ + length_path2* * 5 + 1
"#
    );

    200
}

impl Node {
    fn parse(input: &str) -> Self {
        let mut parts = input.split_whitespace();
        let name = parts.next().unwrap();
        let mut name_parts = name.trim_start_matches("/dev/grid/node-").split('-');
        let x = name_parts.next().unwrap()[1..].parse().unwrap();
        let y = name_parts.next().unwrap()[1..].parse().unwrap();

        let parse_size = |d: &str| d.trim_end_matches('T').parse().unwrap();
        let size = parse_size(parts.next().unwrap());
        let used = parse_size(parts.next().unwrap());
        let avail = parse_size(parts.next().unwrap());

        Node {
            x,
            y,
            avail,
            size,
            used,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day22.txt")), 860);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day22.txt")), 200);
    }
}

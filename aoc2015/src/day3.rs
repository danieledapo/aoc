use std::collections::HashSet;

pub fn part1(input: &str) -> usize {
    visit(input.chars()).len()
}

pub fn part2(input: &str) -> usize {
    let visit_skipping = |parity| {
        visit(
            input
                .chars()
                .enumerate()
                .filter(|(i, _)| i % 2 == parity)
                .map(|(_, c)| c),
        )
    };

    let santa_visited = visit_skipping(0);
    let robo_santa_visited = visit_skipping(1);

    santa_visited.union(&robo_santa_visited).count()
}

fn visit(path: impl IntoIterator<Item = char>) -> HashSet<(i32, i32)> {
    let mut pos = (0, 0);

    let mut visited = HashSet::new();
    visited.insert(pos);

    for c in path {
        pos = match c {
            '^' => (pos.0, pos.1 - 1),
            '>' => (pos.0 + 1, pos.1),
            'v' => (pos.0, pos.1 + 1),
            '<' => (pos.0 - 1, pos.1),
            _ => continue,
        };

        visited.insert(pos);
    }

    visited
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(2565, part1(include_str!("../input/day3.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(2639, part2(include_str!("../input/day3.txt")));
    }
}

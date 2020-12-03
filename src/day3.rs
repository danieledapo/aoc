pub fn part1(input: &str) -> usize {
    let trees = parse_trees(input);

    hit_trees(&trees, 3, 1)
}

pub fn part2(input: &str) -> usize {
    let trees = parse_trees(input);

    hit_trees(&trees, 1, 1)
        * hit_trees(&trees, 3, 1)
        * hit_trees(&trees, 5, 1)
        * hit_trees(&trees, 7, 1)
        * hit_trees(&trees, 1, 2)
}

fn hit_trees(trees: &[Vec<bool>], dx: usize, dy: usize) -> usize {
    let mut ntrees = 0;

    let mut x = 0;
    let mut y = 0;

    while let Some(row) = trees.get(y) {
        if y == 0 {
            x = row.iter().position(|t| !t).unwrap();
        } else {
            x = (x + dx) % row.len();

            if row[x % row.len()] {
                ntrees += 1;
            }
        }

        y += dy;
    }

    ntrees
}

fn parse_trees(input: &str) -> Vec<Vec<bool>> {
    input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '.' => false,
                    '#' => true,
                    _ => panic!("unknown char {}", c),
                })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(164, part1(include_str!("../input/day3.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(5_007_658_656, part2(include_str!("../input/day3.txt")));
    }
}

use std::collections::{BinaryHeap, HashSet};

pub fn part1(magic: i64, target: (i64, i64)) -> i64 {
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();

    queue.push((0, (1, 1)));

    while let Some((d, (x, y))) = queue.pop() {
        let d = -d;

        if (x, y) == target {
            return d;
        }

        if !visited.insert((x, y)) {
            continue;
        }

        for &(x, y) in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)].iter() {
            if x < 0 || y < 0 {
                continue;
            }

            if wall_at(x, y, magic) {
                continue;
            }

            if !visited.contains(&(x, y)) {
                queue.push((-(d + 1), (x, y)));
            }
        }
    }

    unreachable!()
}

pub fn part2(magic: i64, steps: i64) -> usize {
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();

    queue.push((0, (1, 1)));

    while let Some((d, (x, y))) = queue.pop() {
        let d = -d;

        if d > steps {
            break;
        }

        if !visited.insert((x, y)) {
            continue;
        }

        for &(x, y) in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)].iter() {
            if x < 0 || y < 0 {
                continue;
            }

            if wall_at(x, y, magic) {
                continue;
            }

            if !visited.contains(&(x, y)) {
                queue.push((-(d + 1), (x, y)));
            }
        }
    }

    visited.len()
}

fn wall_at(x: i64, y: i64, magic: i64) -> bool {
    let n = x.pow(2) + 3 * x + 2 * x * y + y + y.pow(2) + magic;

    (n.count_ones() & 1) == 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(1358, (31, 39)), 96);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(1358, 50), 141);
    }
}

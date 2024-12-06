use std::{cmp::Reverse, collections::BinaryHeap};

pub fn part1(input: &str) -> u32 {
    let grid = parse(input);

    lowest_risk_score(&grid)
}

pub fn part2(input: &str) -> u32 {
    let mut grid = parse(input);

    for row in &mut grid {
        let n = row.len();
        for x in 0..n * 4 {
            let i = x / n * n + x % n;
            let v = (row[i] + 1) % 10;
            row.push(v.max(1));
        }
    }

    let n = grid.len();
    for y in 0..n * 4 {
        let mut row = vec![];
        let i = y / n * n + y % n;
        for &v in &grid[i] {
            row.push(u32::max(1, (v + 1) % 10));
        }
        grid.push(row);
    }

    lowest_risk_score(&grid)
}

fn lowest_risk_score(grid: &[Vec<u32>]) -> u32 {
    let mut graph = vec![];
    for l in grid {
        graph.push(vec![u32::max_value(); l.len()]);
    }

    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), 0, 0));

    while let Some((Reverse(cost), x, y)) = queue.pop() {
        if cost > graph[y][x] {
            continue;
        }

        if y + 1 == grid.len() && x + 1 == grid[y].len() {
            return cost;
        }

        if x > 0 && grid[y][x - 1] + cost < graph[y][x - 1] {
            graph[y][x - 1] = grid[y][x - 1] + cost;
            queue.push((Reverse(graph[y][x - 1]), x - 1, y));
        }

        if x + 1 < grid[y].len() && grid[y][x + 1] + cost < graph[y][x + 1] {
            graph[y][x + 1] = grid[y][x + 1] + cost;
            queue.push((Reverse(graph[y][x + 1]), x + 1, y));
        }

        if y > 0 && grid[y - 1][x] + cost < graph[y - 1][x] {
            graph[y - 1][x] = grid[y - 1][x] + cost;
            queue.push((Reverse(graph[y - 1][x]), x, y - 1));
        }

        if y + 1 < grid.len() && grid[y + 1][x] + cost < graph[y + 1][x] {
            graph[y + 1][x] = grid[y + 1][x] + cost;
            queue.push((Reverse(graph[y + 1][x]), x, y + 1));
        }
    }

    unreachable!()
}

fn parse(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(487, part1(include_str!("../input/day15.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(2821, part2(include_str!("../input/day15.txt")));
    }
}

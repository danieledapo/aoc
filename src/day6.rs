use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> u32 {
    let points = parse_points(input);
    let (min_x, min_y, max_x, max_y) = bounding_box(&points);

    let mut areas = HashMap::new();
    let mut infinite = HashSet::new();

    for (x, y) in points_iter(min_x, min_y, max_x, max_y) {
        let mut best_dist = i32::max_value();
        let mut best_pt = None;

        for &(cx, cy) in &points {
            let d = distance(x, y, cx, cy);

            if best_dist == d {
                best_pt = None;
            } else if d < best_dist {
                best_dist = d;
                best_pt = Some((cx, cy));
            }
        }

        if let Some(pt) = best_pt {
            if x == min_x || x == max_x || y == min_y || y == max_y {
                infinite.insert(pt);
            }
            *areas.entry(pt).or_default() += 1;
        }
    }

    areas
        .iter()
        .filter(|&(p, _)| !infinite.contains(p))
        .map(|(_, &a)| a)
        .max()
        .unwrap()
}

pub fn part2(input: &str, max: i32) -> usize {
    let points = parse_points(input);
    let (min_x, min_y, max_x, max_y) = bounding_box(&points);

    points_iter(min_x, min_y, max_x, max_y)
        .filter(|&(x, y)| {
            points
                .iter()
                .map(|&(px, py)| distance(x, y, px, py))
                .sum::<i32>()
                < max
        })
        .count()
}

fn distance(x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    (x1 - x2).abs() + (y1 - y2).abs()
}

fn points_iter(min_x: i32, min_y: i32, max_x: i32, max_y: i32) -> impl Iterator<Item = (i32, i32)> {
    (min_x..max_x).flat_map(move |x| (min_y..=max_y).map(move |y| (x, y)))
}

fn parse_points(input: &str) -> Vec<(i32, i32)> {
    input
        .lines()
        .map(|l| {
            let mut parts = l.split(", ");
            let x = parts.next().unwrap().parse::<i32>().unwrap();
            let y = parts.next().unwrap().parse::<i32>().unwrap();

            (x, y)
        })
        .collect::<Vec<_>>()
}

fn bounding_box(points: &[(i32, i32)]) -> (i32, i32, i32, i32) {
    points.iter().fold(
        (
            i32::max_value(),
            i32::max_value(),
            i32::min_value(),
            i32::min_value(),
        ),
        |(mix, miy, max, may), &(x, y)| (mix.min(x), miy.min(y), max.max(x), may.max(y)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(
            17,
            part1(
                r"1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"
            )
        );
        assert_eq!(4011, part1(include_str!("../input/day6.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(
            16,
            part2(
                r"1, 1
1, 6
8, 3
3, 4
5, 5
8, 9",
                32
            )
        );
        assert_eq!(46054, part2(include_str!("../input/day6.txt"), 10000));
    }
}

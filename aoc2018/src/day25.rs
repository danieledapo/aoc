type Point = (i64, i64, i64, i64);

pub fn part1(input: &str) -> usize {
    let points = input
        .trim()
        .lines()
        .map(|l| parse_point(l).unwrap())
        .collect::<Vec<_>>();

    let costellations = find_clusters(points);

    costellations.len()
}

pub fn part2() -> &'static str {
    "Merry Christmas!"
}

fn find_clusters(mut points: Vec<Point>) -> Vec<Vec<Point>> {
    let mut clusters = vec![];

    while let Some(p) = points.pop() {
        let mut current_cluster = vec![];
        let mut stack = vec![p];

        while let Some(cp) = stack.pop() {
            let mut i = 0;

            while i < points.len() {
                if dist(&cp, &points[i]) <= 3 {
                    stack.push(points.swap_remove(i));
                } else {
                    i += 1;
                }
            }

            current_cluster.push(cp);
        }

        clusters.push(current_cluster);
    }

    clusters
}

fn dist(p0: &Point, p1: &Point) -> i64 {
    (p0.0 - p1.0).abs() + (p0.1 - p1.1).abs() + (p0.2 - p1.2).abs() + (p0.3 - p1.3).abs()
}

fn parse_point(line: &str) -> Option<Point> {
    let mut parts = line.split(',');

    let x = parts.next()?.parse().ok()?;
    let y = parts.next()?.parse().ok()?;
    let z = parts.next()?.parse().ok()?;
    let t = parts.next()?.parse().ok()?;

    Some((x, y, z, t))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(399, part1(include_str!("../input/day25.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!("Merry Christmas!", part2());
    }
}

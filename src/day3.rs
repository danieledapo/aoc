use std::collections::HashMap;

type Point = (i32, i32);

pub fn part1(paths: &str) -> i32 {
    let mut lines = paths.lines();

    let path1 = follow_path(parse_path(lines.next().unwrap()));
    let path2 = follow_path(parse_path(lines.next().unwrap()));

    let pts = if path1.len() < path2.len() {
        path1.keys()
    } else {
        path2.keys()
    };

    pts.filter(|p| path1.contains_key(p) && path2.contains_key(p))
        .map(|(ix, iy)| ix.abs() + iy.abs())
        .min()
        .unwrap()
}

pub fn part2(paths: &str) -> i32 {
    let mut lines = paths.lines();

    let path1 = follow_path(parse_path(lines.next().unwrap()));
    let path2 = follow_path(parse_path(lines.next().unwrap()));

    let pts = if path1.len() < path2.len() {
        path1.keys()
    } else {
        path2.keys()
    };

    pts.filter(|p| path1.contains_key(p) && path2.contains_key(p))
        .map(|p| path1[p] + path2[p])
        .min()
        .unwrap()
}

fn follow_path(path: impl IntoIterator<Item = (Point, i32)>) -> HashMap<Point, i32> {
    let mut pts = HashMap::with_capacity(200_000);

    let mut cur = (0, 0);
    let mut t = 0;

    for (dir, c) in path {
        for _ in 0..c {
            cur.0 += dir.0;
            cur.1 += dir.1;
            t += 1;

            pts.entry(cur).or_insert(t);
        }
    }

    pts
}

fn parse_path<'a>(path: &'a str) -> impl Iterator<Item = (Point, i32)> + 'a {
    path.split(',').map(|m| {
        let c = m[1..].parse::<i32>().unwrap();

        let dir = match m.chars().next().unwrap() {
            'R' => (1, 0),
            'L' => (-1, 0),
            'U' => (0, -1),
            'D' => (0, 1),
            _ => unreachable!(),
        };

        (dir, c)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day3.txt")), 227);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day3.txt")), 20286);
    }
}

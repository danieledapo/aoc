use std::collections::HashSet;

use itertools::Itertools;

pub type Scan = Vec<[i64; 3]>;
pub type Transform = [(usize, i64); 3];

const TRANSFORMATIONS: [[(usize, i64); 3]; 24] = [
    [(0, 1), (1, 1), (2, 1)],
    [(0, 1), (2, 1), (1, -1)],
    [(0, 1), (1, -1), (2, -1)],
    [(0, 1), (2, -1), (1, 1)],
    [(0, -1), (1, 1), (2, -1)],
    [(0, -1), (2, 1), (1, 1)],
    [(0, -1), (1, -1), (2, 1)],
    [(0, -1), (2, -1), (1, -1)],
    [(1, 1), (0, 1), (2, -1)],
    [(1, 1), (2, -1), (0, -1)],
    [(1, 1), (0, -1), (2, 1)],
    [(1, 1), (2, 1), (0, 1)],
    [(1, -1), (0, 1), (2, 1)],
    [(1, -1), (2, 1), (0, -1)],
    [(1, -1), (0, -1), (2, -1)],
    [(1, -1), (2, -1), (0, 1)],
    [(2, 1), (1, 1), (0, -1)],
    [(2, 1), (0, -1), (1, -1)],
    [(2, 1), (1, -1), (0, 1)],
    [(2, 1), (0, 1), (1, 1)],
    [(2, -1), (1, 1), (0, 1)],
    [(2, -1), (0, 1), (1, -1)],
    [(2, -1), (1, -1), (0, -1)],
    [(2, -1), (0, -1), (1, 1)],
];

pub fn part1(input: &str) -> usize {
    stitch(parse(input)).0
}

pub fn part2(input: &str) -> i64 {
    let positions = stitch(parse(input)).1;

    positions
        .iter()
        .combinations(2)
        .map(|ps| {
            let [x0, y0, z0] = ps[0];
            let [x1, y1, z1] = ps[1];
            i64::abs(x0 - x1) + i64::abs(y0 - y1) + i64::abs(z0 - z1)
        })
        .max()
        .unwrap()
}

fn stitch(mut scans: Vec<Scan>) -> (usize, Vec<[i64; 3]>) {
    let mut probes: HashSet<_> = scans[0].iter().copied().collect();
    let mut detected = vec![scans.swap_remove(0)];
    let mut positions = vec![[0, 0, 0]];

    while !scans.is_empty() {
        let (i, (xformed, offset)) = scans
            .iter()
            .enumerate()
            .find_map(|(i, scan)| {
                let res = find_match(scan, &detected)?;
                Some((i, res))
            })
            .unwrap();

        scans.swap_remove(i);

        probes.extend(xformed.iter().copied());
        detected.push(xformed);
        positions.push(offset);
    }

    (probes.len(), positions)
}

fn find_match(scan: &Scan, detected: &[Scan]) -> Option<(Scan, [i64; 3])> {
    for xform in TRANSFORMATIONS {
        let scan = transform(scan, &xform);

        for detected_scan in detected {
            for origin in detected_scan {
                for &pivot in &scan {
                    let offset = [
                        origin[0] - pivot[0],
                        origin[1] - pivot[1],
                        origin[2] - pivot[2],
                    ];

                    let matching = scan
                        .iter()
                        .filter(|p| {
                            detected_scan.contains(&[
                                p[0] + offset[0],
                                p[1] + offset[1],
                                p[2] + offset[2],
                            ])
                        })
                        .count();

                    if matching < 12 {
                        continue;
                    }

                    let xformed = scan
                        .iter()
                        .map(|p| [p[0] + offset[0], p[1] + offset[1], p[2] + offset[2]])
                        .collect();

                    return Some((xformed, offset));
                }
            }
        }
    }

    None
}

fn transform(input: &Scan, xform: &Transform) -> Scan {
    let mut res = Vec::with_capacity(input.len());

    for coord in input {
        res.push([
            coord[xform[0].0] * xform[0].1,
            coord[xform[1].0] * xform[1].1,
            coord[xform[2].0] * xform[2].1,
        ]);
    }

    res
}

fn parse(input: &str) -> Vec<Scan> {
    let mut res = vec![];
    let mut cur = Scan::new();
    for l in input.lines() {
        if l.trim().is_empty() || l.starts_with("--- scanner") {
            if !cur.is_empty() {
                res.push(cur);
                cur = Scan::new();
            }
            continue;
        }

        let mut p = l.split(',').map(|d| d.parse().unwrap());
        let x = p.next().unwrap();
        let y = p.next().unwrap();
        let z = p.next().unwrap();

        cur.push([x, y, z]);
    }

    if !cur.is_empty() {
        res.push(cur);
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(434, part1(include_str!("../input/day19.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(11906, part2(include_str!("../input/day19.txt")));
    }
}

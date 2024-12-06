use std::collections::HashSet;

const NEIGHBORS: [(i64, i64, i64); 6] = [
    (-1, 0, 0),
    (1, 0, 0),
    (0, -1, 0),
    (0, 1, 0),
    (0, 0, -1),
    (0, 0, 1),
];

pub fn part1(input: &str) -> u64 {
    let voxels = parse(input);
    count(&voxels)
}

pub fn part2(input: &str) -> u64 {
    let mut voxels = parse(input);

    let (mut minx, mut miny, mut minz) = (i64::MAX, i64::MAX, i64::MAX);
    let (mut maxx, mut maxy, mut maxz) = (i64::MIN, i64::MIN, i64::MIN);

    for &(x, y, z) in &voxels {
        minx = minx.min(x);
        miny = miny.min(y);
        minz = minz.min(z);

        maxx = maxx.max(x);
        maxy = maxy.max(y);
        maxz = maxz.max(z);
    }

    let mut stack = vec![];
    for y in miny..=maxy {
        for x in minx..=maxx {
            stack.extend([(x, y, minz), (x, y, maxz)]);
        }
    }
    for z in minz..=maxz {
        for y in miny..=maxy {
            stack.extend([(minx, y, z), (maxx, y, z)]);
        }
    }
    for z in minz..=maxz {
        for x in minx..=maxx {
            stack.extend([(x, miny, z), (x, maxy, z)]);
        }
    }

    let mut reachable = HashSet::new();
    while let Some((x, y, z)) = stack.pop() {
        if voxels.contains(&(x, y, z)) {
            continue;
        }

        if !reachable.insert((x, y, z)) {
            continue;
        }

        stack.extend(
            NEIGHBORS
                .iter()
                .map(|(dx, dy, dz)| (x + dx, y + dy, z + dz))
                .filter(|(x, y, z)| {
                    (minx..=maxx).contains(x)
                        && (miny..=maxy).contains(y)
                        && (minz..=maxz).contains(z)
                }),
        );
    }

    for z in minz..=maxz {
        for y in miny..=maxy {
            for x in minx..=maxx {
                if reachable.contains(&(x, y, z)) {
                    continue;
                }

                voxels.insert((x, y, z));
            }
        }
    }

    count(&voxels)
}

fn count(voxels: &HashSet<(i64, i64, i64)>) -> u64 {
    let mut faces = 0;
    for &(x, y, z) in voxels {
        for (dx, dy, dz) in NEIGHBORS {
            if !voxels.contains(&(x + dx, y + dy, z + dz)) {
                faces += 1;
            }
        }
    }
    faces
}

fn parse(input: &str) -> HashSet<(i64, i64, i64)> {
    input
        .lines()
        .map(|l| {
            let mut ns = l.split(',').map(|n| n.parse().unwrap());
            let x = ns.next().unwrap();
            let y = ns.next().unwrap();
            let z = ns.next().unwrap();

            (x, y, z)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(3412, part1(include_str!("../input/day18.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(2018, part2(include_str!("../input/day18.txt")));
    }
}

use std::collections::HashSet;

#[derive(Debug)]
struct Blueprint {
    id: u32,
    ore_robot: u32,
    clay_robot: u32,
    obsidian_robot: (u32, u32), // (ore, clay)
    geode_robot: (u32, u32),    // (ore, obsidian)
}

pub fn part1(input: &str) -> u32 {
    parse(input).map(|b| b.id * score(&b, 24)).sum()
}

pub fn part2(input: &str) -> u32 {
    parse(input).take(3).map(|b| score(&b, 32)).product()
}

fn score(b: &Blueprint, tt: u32) -> u32 {
    let mut seen = HashSet::new();

    let mut best = 0;
    let mut stack = vec![(0, 0, 0, 0, 1, 0, 0, 0, tt)];

    while let Some(k @ (or, cl, obs, geo, r1, r2, r3, r4, t)) = stack.pop() {
        if !seen.insert(k) {
            continue;
        }

        best = best.max(geo);
        if t == 0 {
            continue;
        }

        if geo + r4 * t + t * (t + 1) / 2 <= best {
            continue;
        }

        stack.push((or + r1, cl + r2, obs + r3, geo + r4, r1, r2, r3, r4, t - 1));

        let max_r1 = b
            .ore_robot
            .max(b.clay_robot)
            .max(b.obsidian_robot.0)
            .max(b.geode_robot.0);
        if r1 < max_r1 && or >= b.ore_robot {
            stack.push((
                or - b.ore_robot + r1,
                cl + r2,
                obs + r3,
                geo + r4,
                r1 + 1,
                r2,
                r3,
                r4,
                t - 1,
            ));
        }

        if r2 < b.obsidian_robot.1 && or >= b.clay_robot {
            stack.push((
                or - b.clay_robot + r1,
                cl + r2,
                obs + r3,
                geo + r4,
                r1,
                r2 + 1,
                r3,
                r4,
                t - 1,
            ));
        }

        if r3 < b.geode_robot.1 && or >= b.obsidian_robot.0 && cl >= b.obsidian_robot.1 {
            stack.push((
                or - b.obsidian_robot.0 + r1,
                cl - b.obsidian_robot.1 + r2,
                obs + r3,
                geo + r4,
                r1,
                r2,
                r3 + 1,
                r4,
                t - 1,
            ));
        }

        if or >= b.geode_robot.0 && obs >= b.geode_robot.1 {
            stack.push((
                or - b.geode_robot.0 + r1,
                cl + r2,
                obs - b.geode_robot.1 + r3,
                geo + r4,
                r1,
                r2,
                r3,
                r4 + 1,
                t - 1,
            ));
        }
    }

    best
}

fn parse(input: &str) -> impl Iterator<Item = Blueprint> + '_ {
    input.lines().map(|l| {
        let mut parts = l.split_whitespace();
        let id = parts.nth(1).unwrap().trim_end_matches(':').parse().unwrap();
        let ore_robot = parts.nth(4).unwrap().parse().unwrap();
        let clay_robot = parts.nth(5).unwrap().parse().unwrap();
        let obs_ore = parts.nth(5).unwrap().parse().unwrap();
        let obs_clay = parts.nth(2).unwrap().parse().unwrap();
        let geo_ore = parts.nth(5).unwrap().parse().unwrap();
        let geo_obs = parts.nth(2).unwrap().parse().unwrap();

        Blueprint {
            id,
            ore_robot,
            clay_robot,
            obsidian_robot: (obs_ore, obs_clay),
            geode_robot: (geo_ore, geo_obs),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(
            33,
            part1(
                r#"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."#
            )
        );
        assert_eq!(1962, part1(include_str!("../input/day19.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(88160, part2(include_str!("../input/day19.txt")));
    }
}

use std::collections::{BTreeSet, HashMap, HashSet};

type Data = HashMap<String, (u64, HashSet<String>)>;
type DistanceMatrix = HashMap<(String, String), u64>;
type Cache = HashMap<(String, u64, BTreeSet<String>, u8), u64>;

pub fn part1(input: &str) -> u64 {
    solve(input, 30, 1)
}

pub fn part2(input: &str) -> u64 {
    solve(input, 26, 2)
}

pub fn solve(input: &str, time: u64, nplayers: u8) -> u64 {
    let data = parse(input);
    let dmatrix = get_distance_matrix(&data);

    let to_visit: BTreeSet<_> = data
        .iter()
        .filter_map(|(n, (f, _))| (*f > 0).then(|| n.clone()))
        .collect();

    fn go(
        data: &Data,
        dmatrix: &DistanceMatrix,
        cache: &mut Cache,
        cur: String,
        time: u64,
        to_visit: BTreeSet<String>,
        player: u8,
        maxtime: u64,
    ) -> u64 {
        let next_player = |cache: &mut Cache, to_visit| {
            go(
                data,
                dmatrix,
                cache,
                "AA".to_string(),
                maxtime,
                to_visit,
                player - 1,
                maxtime,
            )
        };
        if time == 0 {
            if player == 0 {
                return 0;
            } else {
                return next_player(cache, to_visit);
            }
        }

        let k = (cur.clone(), time, to_visit.clone(), player);
        if let Some(c) = cache.get(&k) {
            return *c;
        }

        let mut best = 0;

        for dst in &to_visit {
            let d = dmatrix[&(cur.to_string(), dst.to_string())];
            if time <= d + 1 {
                if player > 0 {
                    best = best.max(next_player(cache, to_visit.clone()));
                }
                continue;
            }

            let newt = time - d - 1;
            let newscore = data[dst].0 * newt;

            let mut new_to_visit = to_visit.clone();
            new_to_visit.remove(dst);

            best = best.max(
                newscore
                    + go(
                        data,
                        dmatrix,
                        cache,
                        dst.to_string(),
                        newt,
                        new_to_visit.clone(),
                        player,
                        maxtime,
                    ),
            );
            if player > 0 {
                best = best.max(newscore + next_player(cache, new_to_visit));
            }
        }

        cache.insert(k, best);
        best
    }

    go(
        &data,
        &dmatrix,
        &mut HashMap::new(),
        "AA".to_string(),
        time,
        to_visit,
        nplayers - 1,
        time,
    )
}

fn get_distance_matrix(data: &Data) -> DistanceMatrix {
    // Floyd–Warshall algorithm

    let mut matrix = HashMap::new();

    for (n, (_, neighbors)) in data {
        for m in data.keys() {
            let k = (n.to_string(), m.to_string());
            if n == m {
                matrix.insert(k, 0);
                continue;
            }

            matrix.insert(
                k,
                if neighbors.contains(m) {
                    1
                } else {
                    u64::MAX / 10
                },
            );
        }
    }

    for n0 in data.keys() {
        for n1 in data.keys() {
            for n2 in data.keys() {
                let cd = matrix[&(n1.to_string(), n0.to_string())]
                    + matrix[&(n0.to_string(), n2.to_string())];

                let d = matrix.get_mut(&(n1.to_string(), n2.to_string())).unwrap();
                if cd < *d {
                    *d = cd;
                }
            }
        }
    }

    matrix
}

fn parse(input: &str) -> Data {
    // oh man, I should stop parsing this non-sense, it's easier to "parse" it
    // using multiple selection and manual editing...
    input
        .lines()
        .map(|l| {
            let name = l.split_whitespace().nth(1).unwrap().to_string();

            let f = l.split_once('=').unwrap().1.split_once(';').unwrap().0;
            let flow_rate = f.parse().unwrap();

            // for fuck's sake...
            let neighbors = l
                .split_once("; ")
                .unwrap()
                .1
                .trim_start_matches("tunnels lead to ")
                .trim_start_matches("tunnel leads to ")
                .trim_start_matches("valve ")
                .trim_start_matches("valves ")
                .split(", ")
                .map(|a| a.to_string())
                .collect();

            (name, (flow_rate, neighbors))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(
            1651,
            part1(
                r#"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"#
            )
        );
        assert_eq!(1923, part1(include_str!("../input/day16.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(
            1707,
            part2(
                r#"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"#
            )
        );
        assert_eq!(2594, part2(include_str!("../input/day16.txt")));
    }
}

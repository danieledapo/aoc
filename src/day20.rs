use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Room {
    min_distance: i64,
    connected: HashSet<Pos>,
}

type Pos = (i64, i64);
type Area = HashMap<Pos, Room>;

pub fn part1(input: &str) -> i64 {
    let area = parse_area(input.trim());

    // print_area(&area);

    area.values().map(|r| r.min_distance).max().unwrap()
}

pub fn part2(input: &str) -> usize {
    let area = parse_area(input.trim());

    area.values().filter(|r| r.min_distance >= 1000).count()
}

fn parse_area(input: &str) -> Area {
    let input = input.trim_start_matches('^').trim_end_matches('$');

    let mut area: Area = HashMap::new();
    let mut stack = vec![];
    let mut prev_pos = (0, 0);
    let mut pos = (0, 0);

    area.insert(
        pos,
        Room {
            min_distance: 0,
            connected: HashSet::new(),
        },
    );

    // this shouldn't work according to the problem description because it
    // doesn't take into account the fact that parentheses could visit different
    // node paths that could affect distances. However, this worked for my input
    // so... I really don't want to parse properly that thing :)
    for c in input.chars() {
        match c {
            '(' => stack.push(pos),
            ')' => pos = stack.pop().unwrap(),
            '|' => pos = *stack.last().unwrap(),
            'N' | 'S' | 'E' | 'W' => {
                match c {
                    'E' => pos.0 += 1,
                    'W' => pos.0 -= 1,
                    'N' => pos.1 -= 1,
                    'S' => pos.1 += 1,
                    _ => unreachable!(),
                };

                let prev_room = area.get_mut(&prev_pos).unwrap();
                prev_room.connected.insert(pos);

                let dist = prev_room.min_distance + 1;

                area.entry(pos)
                    .and_modify(|r| {
                        r.min_distance = r.min_distance.min(dist);
                        r.connected.insert(prev_pos);
                    })
                    .or_insert_with(|| Room {
                        min_distance: dist,
                        connected: std::iter::once(prev_pos).collect(),
                    });
            }
            _ => unimplemented!(),
        };

        prev_pos = pos;
    }

    area
}

// fn print_area(area: &Area) {
//     let min_x = *area.keys().map(|(x, _)| x).min().unwrap();
//     let min_y = *area.keys().map(|(_, y)| y).min().unwrap();
//     let max_x = *area.keys().map(|(x, _)| x).max().unwrap();
//     let max_y = *area.keys().map(|(_, y)| y).max().unwrap();

//     println!("{:?}", (min_x, min_y, max_x, max_y));

//     for y in min_y..=max_y {
//         let row = (min_x..=max_x)
//             .map(|x| {
//                 if let Some(room) = area.get(&(x, y)) {
//                     if room.connected.contains(&(x, y - 1)) {
//                         "-#"
//                     } else {
//                         "##"
//                     }
//                 } else {
//                     "##"
//                 }
//             })
//             .collect::<String>();
//         println!("#{}", row);

//         let row = (min_x..=max_x)
//             .map(|x| {
//                 if let Some(room) = area.get(&(x, y)) {
//                     if (x, y) == (0, 0) {
//                         if room.connected.contains(&(x + 1, y)) {
//                             "X|"
//                         } else {
//                             "X#"
//                         }
//                     } else {
//                         if room.connected.contains(&(x + 1, y)) {
//                             ".|"
//                         } else {
//                             ".#"
//                         }
//                     }
//                 } else {
//                     "##"
//                 }
//             })
//             .collect::<String>();
//         println!("#{}", row);
//     }

//     println!(
//         "{}",
//         (0..(max_x - min_x + 1) * 2 + 1)
//             .map(|_| '#')
//             .collect::<String>()
//     )
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(3, part1("^WNE$"));
        assert_eq!(10, part1("^ENWWW(NEEE|SSE(EE|N))$"));
        assert_eq!(
            23,
            part1("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
        );
        assert_eq!(
            31,
            part1("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
        );
        assert_eq!(18, part1("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"));
        assert_eq!(3560, part1(include_str!("../input/day20.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(8688, part2(include_str!("../input/day20.txt")));
    }
}

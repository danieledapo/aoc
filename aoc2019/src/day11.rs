use std::collections::HashMap;

use crate::day5;

pub fn part1(input: &str) -> usize {
    paint(input, 0).len()
}

pub fn part2(input: &str) -> String {
    let hull = paint(input, 1);

    let mut minx = i64::max_value();
    let mut miny = i64::max_value();
    let mut maxx = i64::min_value();
    let mut maxy = i64::min_value();

    for (&(x, y), &c) in &hull {
        if c == 0 {
            continue;
        }

        minx = minx.min(x);
        miny = miny.min(y);
        maxx = maxx.max(x);
        maxy = maxy.max(y);
    }

    let mut lines = vec![];
    for y in miny..=maxy {
        let line: String = (minx..=maxx)
            .map(|x| match hull.get(&(x, y)).unwrap_or(&0) {
                0 => ' ',
                1 => '*',
                _ => unreachable!(),
            })
            .collect();

        lines.push(line);
    }

    lines.join("\n")
}

fn paint(input: &str, initial_color: i64) -> HashMap<(i64, i64), i64> {
    let mut prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldOutput);
    let mut visited: HashMap<(i64, i64), i64> = HashMap::new();

    let mut p = (0, 0);
    let mut dir = (0, -1);

    visited.insert(p, initial_color);

    loop {
        let cur_color = *visited.get(&p).unwrap_or(&0);
        let mut inputs = vec![cur_color];

        let output = prog.run(&mut inputs);
        if output.is_empty() {
            break;
        }

        let new_color = output[0];
        let turn_dir = prog.run(&mut inputs)[0];

        visited.insert(p, new_color);

        dir = match turn_dir {
            1 => (-dir.1, dir.0),
            0 => (dir.1, -dir.0),
            _ => unreachable!(),
        };

        p.0 += dir.0;
        p.1 += dir.1;
    }

    visited
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day11.txt")), 2478);
    }

    #[test]
    fn test_part2() {
        let msg = part2(include_str!("../input/day11.txt"));
        assert_eq!(
            msg,
            r#"
*  *  **  **** ***  *  *  **   **  ****
*  * *  *    * *  * *  * *  * *  *    *
**** *      *  *  * *  * *    *  *   * 
*  * *     *   ***  *  * * ** ****  *  
*  * *  * *    * *  *  * *  * *  * *   
*  *  **  **** *  *  **   *** *  * ****"#
                .trim_start(),
            "\n{}",
            msg
        );
    }
}

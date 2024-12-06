use std::convert::TryFrom;

use crate::day5;

pub fn part1(input: &str) -> i64 {
    let prog = day5::Machine::with_run_mode(input, day5::RunMode::Yield);

    let mut nics: Vec<_> = (0..50).map(|id| (prog.clone(), vec![id])).collect();
    loop {
        for i in 0..nics.len() {
            if nics[i].1.is_empty() {
                nics[i].1.push(-1);
            }

            let (prog, inputs) = &mut nics[i];

            let outputs = prog.run(inputs);
            if outputs.is_empty() {
                continue;
            }

            let a = usize::try_from(outputs[0]).unwrap();
            let x = prog.run(inputs)[0];
            let y = prog.run(inputs)[0];

            if a == 255 {
                return y;
            }

            nics[a].1.push(x);
            nics[a].1.push(y);
        }
    }
}

pub fn part2(input: &str) -> i64 {
    let prog = day5::Machine::with_run_mode(input, day5::RunMode::Yield);

    let mut nics: Vec<_> = (0..50).map(|id| (prog.clone(), vec![id])).collect();
    let mut last_nat_xy = (0, 0);
    let mut last_nat_sent_y = 1;
    loop {
        let mut idle = true;

        for i in 0..nics.len() {
            if nics[i].1.is_empty() {
                nics[i].1.push(-1);
            }

            let (prog, inputs) = &mut nics[i];

            let outputs = prog.run(inputs);
            if outputs.is_empty() {
                continue;
            }

            idle = false;

            let a = usize::try_from(outputs[0]).unwrap();
            let x = prog.run(inputs)[0];
            let y = prog.run(inputs)[0];

            if a == 255 {
                last_nat_xy = (x, y);
                continue;
            }

            nics[a].1.push(x);
            nics[a].1.push(y);
        }

        if idle {
            if last_nat_xy.1 == last_nat_sent_y {
                return last_nat_sent_y;
            }

            nics[0].1.push(last_nat_xy.0);
            nics[0].1.push(last_nat_xy.1);

            last_nat_sent_y = last_nat_xy.1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day23.txt")), 26744);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day23.txt")), 19498);
    }
}

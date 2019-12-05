use std::convert::TryFrom;

pub fn part1(inp: &str) -> i32 {
    let mut prog = parse(inp);

    *run_til_halt(&mut prog, &[1]).last().unwrap()
}

pub fn part2(inp: &str) -> i32 {
    let mut prog = parse(inp);

    *run_til_halt(&mut prog, &[5]).last().unwrap()
}

pub fn parse(inp: &str) -> Vec<i32> {
    inp.trim().split(',').map(|n| n.parse().unwrap()).collect()
}

pub fn run_til_halt(prog: &mut [i32], inputs: &[i32]) -> Vec<i32> {
    let get_value = |prog: &mut [i32], arg: i32, mode: i32| match mode {
        0 => prog[usize::try_from(arg).unwrap()],
        1 => arg,
        _ => unreachable!(),
    };

    let mut ip = 0;
    let mut inputs = inputs.iter();
    let mut outputs = vec![];

    while prog[ip] != 99 {
        let op = prog[ip] % 100;
        let mode1 = prog[ip] / 100 % 10;
        let mode2 = prog[ip] / 1_000 % 10;

        if op == 1 || op == 2 {
            let im1 = get_value(prog, prog[ip + 1], mode1);
            let im2 = get_value(prog, prog[ip + 2], mode2);
            let dst = usize::try_from(prog[ip + 3]).unwrap();

            prog[dst] = match op {
                1 => im1 + im2,
                2 => im1 * im2,
                _ => unreachable!(),
            };

            ip += 4;
            continue;
        }

        if op == 3 {
            let dst = usize::try_from(prog[ip + 1]).unwrap();
            prog[dst] = *inputs.next().unwrap();
            ip += 2;
            continue;
        }

        if op == 4 {
            outputs.push(get_value(prog, prog[ip + 1], mode1));
            ip += 2;
            continue;
        }

        if op == 5 {
            if get_value(prog, prog[ip + 1], mode1) != 0 {
                ip = usize::try_from(get_value(prog, prog[ip + 2], mode2)).unwrap();
            } else {
                ip += 3;
            }
            continue;
        }

        if op == 6 {
            if get_value(prog, prog[ip + 1], mode1) == 0 {
                ip = usize::try_from(get_value(prog, prog[ip + 2], mode2)).unwrap();
            } else {
                ip += 3;
            }
            continue;
        }

        if op == 7 {
            let dst = usize::try_from(prog[ip + 3]).unwrap();
            prog[dst] =
                if get_value(prog, prog[ip + 1], mode1) < get_value(prog, prog[ip + 2], mode2) {
                    1
                } else {
                    0
                };
            ip += 4;
            continue;
        }

        if op == 8 {
            let dst = usize::try_from(prog[ip + 3]).unwrap();
            prog[dst] =
                if get_value(prog, prog[ip + 1], mode1) == get_value(prog, prog[ip + 2], mode2) {
                    1
                } else {
                    0
                };
            ip += 4;
            continue;
        }

        unreachable!();
    }

    outputs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day5.txt")), 13818007);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day5.txt")), 3176266);
    }
}

pub fn part1(inp: &str) -> usize {
    let mut prog = parse(inp);

    prog[1] = 12;
    prog[2] = 2;

    run_til_halt(&mut prog);

    prog[0]
}

pub fn part2(inp: &str) -> usize {
    let prog = parse(inp);

    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut prog = prog.clone();

            prog[1] = noun;
            prog[2] = verb;

            run_til_halt(&mut prog);

            if prog[0] == 19_690_720 {
                return noun * 100 + verb;
            }
        }
    }

    unreachable!()
}

pub fn parse(inp: &str) -> Vec<usize> {
    inp.trim().split(',').map(|n| n.parse().unwrap()).collect()
}

pub fn run_til_halt(prog: &mut [usize]) {
    let mut ip = 0;

    while prog[ip] != 99 {
        let op = prog[ip];
        let im1 = prog[prog[ip + 1]];
        let im2 = prog[prog[ip + 2]];
        let dst = prog[ip + 3];

        prog[dst] = match op {
            1 => im1 + im2,
            2 => im1 * im2,
            _ => unreachable!(),
        };

        ip += 4;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day2.txt")), 5866714);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day2.txt")), 5208);
    }
}

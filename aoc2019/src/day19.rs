use crate::day5;

pub fn part1(input: &str) -> usize {
    let prog = day5::Machine::new(input);

    (0..50)
        .flat_map(|y| (0..50).map(move |x| (x, y)))
        .map(|(x, y)| {
            let mut prog = prog.clone();
            let outputs = prog.run(&mut vec![x, y]);
            outputs[0]
        })
        .filter(|r| *r == 1)
        .count()
}

pub fn part2(input: &str, size: i64) -> i64 {
    let prog = day5::Machine::new(input);

    let mut y = size - 1;
    let mut sx = 0;
    loop {
        sx = (sx..)
            .find(|x| prog.clone().run(&mut vec![*x, y])[0] == 1)
            .unwrap();

        // check only corners, it should be enough
        let found = prog.clone().run(&mut vec![sx, y - size + 1])[0] == 1
            && prog.clone().run(&mut vec![sx + size - 1, y])[0] == 1
            && prog.clone().run(&mut vec![sx + size - 1, y - size + 1])[0] == 1;

        if found {
            return sx * 10_000 + y - size + 1;
        }

        y += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day19.txt")), 141);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day19.txt"), 100), 15641348);
    }
}

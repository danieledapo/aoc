use crate::day5;

pub fn part1(inp: &str) -> i64 {
    let mut m = day5::Machine::new(inp);
    let outputs = m.run(&mut vec![1]);
    *outputs.last().unwrap()
}

pub fn part2(inp: &str) -> i64 {
    let mut m = day5::Machine::new(inp);
    let outputs = m.run(&mut vec![2]);
    *outputs.last().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        part1("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");
        part1("1102,34915192,34915192,7,4,7,99,0");
        part1("104,1125899906842624,99");

        assert_eq!(part1(include_str!("../input/day9.txt")), 3598076521);
    }

    #[test]
    fn test_part2() {
        part1("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");
        part1("1102,34915192,34915192,7,4,7,99,0");
        part1("104,1125899906842624,99");

        assert_eq!(part2(include_str!("../input/day9.txt")), 90722);
    }
}

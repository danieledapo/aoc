pub fn part1(input: &str) -> i32 {
    input
        .lines()
        .map(|l| l.parse::<i32>().unwrap())
        .map(fuel_required)
        .sum()
}

pub fn part2(input: &str) -> i32 {
    input
        .lines()
        .map(|l| l.parse::<i32>().unwrap())
        .map(|mut mass| {
            let mut fuel = 0;

            while mass > 0 {
                let f = fuel_required(mass);
                fuel += f;
                mass = f;
            }

            fuel
        })
        .sum()
}

fn fuel_required(mass: i32) -> i32 {
    (mass / 3 - 2).max(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day1.txt")), 3342946);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day1.txt")), 5011553);
    }
}

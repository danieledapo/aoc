use crate::day5;

pub fn interactive(input: &str) {
    let mut prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldInput);

    let mut inputs = vec![];
    loop {
        let outputs = prog.run(&mut inputs);
        for o in outputs {
            if (0..=127).contains(&o) {
                print!("{}", o as u8 as char);
            } else {
                println!("{}", o);
            }
        }

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        inputs.extend(line.chars().map(|c| i64::from(c as u8)));
    }
}

pub fn part1(input: &str, instruction_to_take_good_items_and_reach_floor: &[&str]) -> i64 {
    let prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldInput);

    let items: Vec<_> = instruction_to_take_good_items_and_reach_floor
        .iter()
        .filter(|c| c.starts_with("take "))
        .map(|c| c.trim_start_matches("take "))
        .collect();

    for i in 1..2_usize.pow(items.len() as u32) {
        let mut picked = vec![];

        let mut j = 0;
        let mut n = i;
        while n > 0 {
            if n & 1 == 1 {
                picked.push(items[j]);
            }
            n >>= 1;
            j += 1;
        }

        let n = instruction_to_take_good_items_and_reach_floor.len();
        let mut inputs: Vec<_> = (&instruction_to_take_good_items_and_reach_floor[..n - 1])
            .join(&"\n")
            .chars()
            .map(|c| (i64::from(c as u8)))
            .collect();
        inputs.push(i64::from(b'\n'));

        inputs.extend(items.iter().flat_map(|i| {
            format!("drop {}\n", i)
                .chars()
                .map(|c| i64::from(c as u8))
                .collect::<Vec<_>>()
        }));

        inputs.extend(picked.iter().flat_map(|i| {
            format!("take {}\n", i)
                .chars()
                .map(|c| i64::from(c as u8))
                .collect::<Vec<_>>()
        }));

        inputs.extend(
            instruction_to_take_good_items_and_reach_floor
                .last()
                .unwrap()
                .chars()
                .map(|c| i64::from(c as u8)),
        );
        inputs.push(i64::from(b'\n'));

        let mut prog = prog.clone();
        let output = prog.run(&mut inputs);
        let output: String = output.iter().map(|c| *c as u8 as char).collect();

        if output.contains("Alert!") {
            continue;
        }

        println!("{}", output);
        let haystack = "Oh, hello! You should be able to get in by typing ";
        let number_start = output.find(haystack).unwrap() + haystack.len();
        let number_end = number_start + output[number_start..].find(' ').unwrap();

        return output[number_start..number_end].parse().unwrap();
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(
            part1(
                include_str!("../input/day25.txt"),
                &[
                    "north",
                    "take wreath",
                    "east",
                    "east",
                    "east",
                    "take weather machine",
                    "west",
                    "west",
                    "west",
                    "south",
                    "south",
                    "west",
                    "take prime number",
                    "west",
                    "take astrolabe",
                    "east",
                    "east",
                    "south",
                    "take candy cane",
                    "north",
                    "north",
                    "east",
                    "take food ration",
                    "south",
                    "east",
                    "south",
                    "take hypercube",
                    "east",
                    "take space law space brochure",
                    "north",
                    "west",
                ]
            ),
            2415919488
        );
    }
}

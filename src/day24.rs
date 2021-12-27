use std::collections::BTreeMap;

pub fn part1(input: &str) -> String {
    let digits = run(input);

    digits.values().map(|(_, m)| m.to_string()).collect()
}

pub fn part2(input: &str) -> String {
    let digits = run(input);

    digits.values().map(|(m, _)| m.to_string()).collect()
}

fn run(input: &str) -> BTreeMap<i32, (i32, i32)> {
    let mut digits = BTreeMap::new();

    let mut stack = vec![];
    let mut digit = 0;

    let mut push = false;
    let mut sub = 0;

    for (i, line) in input.lines().enumerate() {
        let val = line.split_whitespace().nth(2);
        if i % 18 == 4 {
            push = val.unwrap() == "1";
            continue;
        }

        if i % 18 == 5 {
            sub = val.unwrap().parse().unwrap();
            continue;
        }

        if i % 18 == 15 {
            if push {
                stack.push((digit, val.unwrap().parse::<i32>().unwrap()));
            } else {
                let (sibling, add) = stack.pop().unwrap();
                let diff = add + sub;
                if diff < 0 {
                    digits.insert(sibling, (-diff + 1, 9));
                    digits.insert(digit, (1, 9 + diff));
                } else {
                    digits.insert(sibling, (1, 9 - diff));
                    digits.insert(digit, (1 + diff, 9));
                }
            }

            digit += 1;
        }
    }

    digits
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!("51983999947999", &part1(include_str!("../input/day24.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!("11211791111365", &part2(include_str!("../input/day24.txt")));
    }
}

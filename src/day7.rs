use std::collections::HashMap;

pub fn part1(input: &str) -> usize {
    fn can_contain<'s>(
        bags: &'s HashMap<&'s str, HashMap<&str, u32>>,
        cache: &mut HashMap<&'s str, bool>,
        bag: &'s str,
        target: &str,
    ) -> bool {
        if bag == target {
            return true;
        }

        if let Some(cc) = cache.get(bag) {
            return *cc;
        }

        let cc = bags[bag]
            .keys()
            .any(|sb| can_contain(bags, cache, sb, target));
        cache.insert(bag, cc);
        cc
    }

    let bags = parse(input);

    let mut cache = HashMap::new();
    bags.iter()
        .filter(|(b, _)| **b != "shiny gold" && can_contain(&bags, &mut cache, b, "shiny gold"))
        .count()
}

pub fn part2(input: &str) -> u32 {
    fn cost<'s>(
        bags: &'s HashMap<&'s str, HashMap<&str, u32>>,
        cache: &mut HashMap<&'s str, u32>,
        bag: &'s str,
    ) -> u32 {
        if let Some(cc) = cache.get(bag) {
            return *cc;
        }

        let c = bags[bag]
            .iter()
            .map(|(sb, units)| units + units * cost(bags, cache, sb))
            .sum();
        cache.insert(bag, c);
        c
    }

    let bags = parse(input);

    let mut cache = HashMap::new();
    cost(&bags, &mut cache, "shiny gold")
}

fn parse(input: &str) -> HashMap<&str, HashMap<&str, u32>> {
    input
        .lines()
        .map(|l| {
            let mut s = l.trim_end_matches('.').splitn(2, " bags contain ");
            let bag = s.next().unwrap();
            let subbags;

            let s = s.next().unwrap();
            if s == "no other bags" {
                subbags = HashMap::new();
            } else {
                subbags = s
                    .split(", ")
                    .map(|sb| {
                        let mut s = sb.splitn(2, ' ');
                        let n = s.next().unwrap().parse::<u32>().unwrap();
                        let b = s
                            .next()
                            .unwrap()
                            .trim_end_matches(" bag")
                            .trim_end_matches(" bags");

                        (b, n)
                    })
                    .collect();
            }

            (bag, subbags)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(119, part1(include_str!("../input/day7.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(155802, part2(include_str!("../input/day7.txt")));
    }
}

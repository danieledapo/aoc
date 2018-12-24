use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Sue<'a> {
    id: usize,
    props: HashMap<&'a str, usize>,
}

pub fn part1(input: &str) -> usize {
    let mut sues = input.lines().map(|l| Sue::parse(l).unwrap());

    sues.find(|sue| {
        sue.prop_count_eq("children", 3)
            && sue.prop_count_eq("cats", 7)
            && sue.prop_count_eq("samoyeds", 2)
            && sue.prop_count_eq("pomeranians", 3)
            && sue.prop_count_eq("akitas", 0)
            && sue.prop_count_eq("vizslas", 0)
            && sue.prop_count_eq("goldfish", 5)
            && sue.prop_count_eq("trees", 3)
            && sue.prop_count_eq("cars", 2)
            && sue.prop_count_eq("perfumes", 1)
    })
    .unwrap()
    .id
}

pub fn part2(input: &str) -> usize {
    let mut sues = input.lines().map(|l| Sue::parse(l).unwrap());

    sues.find(|sue| {
        sue.prop_count_eq("children", 3)
            && *sue.props.get("cats").unwrap_or(&8) > 7
            && *sue.props.get("trees").unwrap_or(&4) > 3
            && *sue.props.get("pomeranians").unwrap_or(&2) < 3
            && *sue.props.get("goldfish").unwrap_or(&4) < 5
            && sue.prop_count_eq("samoyeds", 2)
            && sue.prop_count_eq("akitas", 0)
            && sue.prop_count_eq("vizslas", 0)
            && sue.prop_count_eq("cars", 2)
            && sue.prop_count_eq("perfumes", 1)
    })
    .unwrap()
    .id
}

impl<'a> Sue<'a> {
    fn prop_count_eq(&self, prop: &str, count: usize) -> bool {
        *self.props.get(prop).unwrap_or(&count) == count
    }

    fn parse(input: &'a str) -> Option<Self> {
        let input = input.trim_start_matches("Sue ");

        let parse_num = |n: &str| n.parse::<usize>().ok();

        let first_double_col = input.find(':')?;

        let id = parse_num(&input[0..first_double_col])?;

        let props = (&input[first_double_col + 1..]).split(", ");
        let mut sue_props = HashMap::new();

        for prop in props {
            let mut prop_parts = prop.split(": ");
            let prop_id = prop_parts.next()?.trim();
            let prop_count = parse_num(prop_parts.next()?)?;

            sue_props.insert(prop_id, prop_count);
        }

        Some(Sue {
            id,
            props: sue_props,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(40, part1(include_str!("../input/day16.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(241, part2(include_str!("../input/day16.txt")));
    }
}

use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Chemical<'a> {
    qty: usize,
    name: &'a str,
    input_chemicals: Vec<(&'a str, usize)>,
}

pub fn part1(input: &str) -> usize {
    let chemicals = parse(input);

    let mut leftovers = chemicals.keys().map(|s| (*s, 0)).collect();
    ore_requirement(&chemicals, &mut leftovers, "FUEL", 1)
}

pub fn part2(input: &str) -> usize {
    let chemicals = parse(input);
    let max_ore = 1_000_000_000_000_usize;

    let mut s = 0;
    let mut e = 1_000_000_000_usize;
    let mut max_fuel = 0;
    while s < e {
        let fuel = s + (e - s) / 2;
        let ore = ore_requirement(
            &chemicals,
            &mut chemicals.keys().map(|s| (*s, 0)).collect(),
            "FUEL",
            fuel,
        );

        match max_ore.cmp(&ore) {
            Ordering::Equal => {
                max_fuel = fuel;
                break;
            }
            Ordering::Less => {
                e = fuel;
            }
            Ordering::Greater => {
                max_fuel = fuel;
                s = fuel + 1;
            }
        }
    }

    max_fuel
}

fn ore_requirement(
    chemicals: &HashMap<&str, Chemical>,
    leftovers: &mut HashMap<&str, usize>,
    chemical_name: &str,
    runits: usize,
) -> usize {
    if !chemicals.contains_key(chemical_name) {
        return runits;
    }

    let chemical = chemicals[chemical_name].clone();

    let mut leftover = leftovers[chemical_name];
    let units = runits.saturating_sub(leftover);
    leftover -= runits - units;

    if units > 0 {
        let req = (units + chemical.qty - 1) / chemical.qty * chemical.qty;
        let t = req / chemical.qty;

        *leftovers.get_mut(chemical_name).unwrap() = leftover + (req - units);

        chemical
            .input_chemicals
            .iter()
            .map(|(c, u)| ore_requirement(chemicals, leftovers, c, t * u))
            .sum()
    } else {
        *leftovers.get_mut(chemical_name).unwrap() = leftover;
        0
    }
}

fn parse(input: &str) -> HashMap<&str, Chemical> {
    input
        .lines()
        .map(|l| {
            let mut parts = l.split(" => ");

            let input_chemicals = parts
                .next()
                .unwrap()
                .split(", ")
                .map(parse_chemical)
                .collect();

            let (chemical, qty) = parse_chemical(parts.next().unwrap());

            (
                chemical,
                Chemical {
                    name: chemical,
                    qty,
                    input_chemicals,
                },
            )
        })
        .collect()
}

fn parse_chemical(s: &str) -> (&str, usize) {
    let mut parts = s.split(' ');
    let qty = parts.next().unwrap().parse().unwrap();
    let name = parts.next().unwrap();

    (name, qty)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day14.txt")), 387001);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day14.txt")), 3412429);
    }
}

use std::collections::{HashMap, HashSet};

type Rules<'a> = HashMap<&'a str, Vec<&'a str>>;

pub fn part1(input: &str) -> usize {
    let (rules, molecule) = parse_input(input).unwrap();

    let all_possible_molecules = possible_molecules(molecule, &rules);

    all_possible_molecules
        .into_iter()
        .collect::<HashSet<_>>()
        .len()
}

pub fn part2(input: &str) -> usize {
    // I had no idea how to solve this in reasonable time, because a naive bfs
    // would take too long. I resorted to the internet :). It turns out there's
    // a pattern in the rules. In particular, uppercase letters are molecules
    // while lowercase ones are something else. Almost every rule always adds a
    // molecule. However, there are "special" molecules: "Rn"  and "Ar" that do
    // not actually count as molecules but can be seen as enclosing the real new
    // molecule possibly separated by a "Y" which is kind of a ",".
    //
    // The path from the molecule and the start molecule 'e' is then the number
    // of elements - rn * 2 - y * 2 - 1
    //
    // The final -1 is because we want to start from e.
    //
    // Thanks internet :).
    //

    let (_, molecule) = parse_input(input).unwrap();

    let elements = molecule.chars().filter(|c| *c >= 'A' && *c <= 'Z').count();
    let rn = molecule.matches("Rn").count();
    let y = molecule.chars().filter(|c| *c == 'Y').count();
    let ar = molecule.matches("Ar").count();

    assert!(rn == ar);

    elements - rn * 2 - y * 2 - 1
}

fn possible_molecules(molecule: &str, rules: &Rules) -> Vec<String> {
    let mut next = vec![];

    for (pat, new_pats) in rules.iter() {
        for (i, mat) in molecule.match_indices(pat) {
            for new_pat in new_pats {
                let mut new_molecule = molecule.to_owned();
                new_molecule.replace_range(i..i + mat.len(), new_pat);
                next.push(new_molecule);
            }
        }
    }

    next
}

fn parse_input(input: &str) -> Option<(Rules, &str)> {
    let mut parts = input.trim().split("\n\n");

    let mut rules: Rules = HashMap::new();
    for rule in parts.next()?.lines() {
        let mut lparts = rule.split(" => ");

        let src = lparts.next()?;
        let dst = lparts.next()?;

        rules.entry(src).or_default().push(dst);
    }

    let molecule = parts.next()?;

    Some((rules, molecule))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(576, part1(include_str!("../input/day19.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(207, part2(include_str!("../input/day19.txt")));
    }
}

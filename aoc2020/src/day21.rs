use std::collections::{HashMap, HashSet};

type Food<'s> = (HashSet<&'s str>, HashSet<&'s str>);

pub fn part1(input: &str) -> u32 {
    let foods = parse(input);

    let all_ingredients = foods
        .iter()
        .flat_map(|(ing, _)| ing.iter().copied())
        .collect::<HashSet<_>>();

    let known_allergens = find_allergens(&foods);

    all_ingredients
        .iter()
        .map(|ing| {
            if known_allergens.contains_key(ing) {
                return 0;
            }

            foods
                .iter()
                .map(|(ings, _)| if ings.contains(ing) { 1 } else { 0 })
                .sum()
        })
        .sum()
}

pub fn part2(input: &str) -> String {
    let foods = parse(input);
    let mut known_allergens = find_allergens(&foods).into_iter().collect::<Vec<_>>();
    known_allergens.sort_by_key(|(_, alle)| *alle);

    let mut s = String::new();
    for (i, (ing, _)) in known_allergens.iter().enumerate() {
        s += ing;
        if i != known_allergens.len() - 1 {
            s += ",";
        }
    }

    s
}

fn find_allergens<'a>(foods: &'a [Food]) -> HashMap<&'a str, &'a str> {
    let mut all_allergens: HashSet<&str> = HashSet::new();
    let mut all_ingredients: HashSet<&str> = HashSet::new();

    for (ings, alls) in foods {
        all_allergens.extend(alls.iter());
        all_ingredients.extend(ings.iter());
    }

    let mut potential_ingredients = HashMap::new();
    for allergen in &all_allergens {
        let mut allergen_ings = all_ingredients.clone();
        for (ings, alls) in foods {
            if alls.contains(allergen) {
                allergen_ings = allergen_ings.intersection(ings).copied().collect();
            }
        }

        potential_ingredients.insert(*allergen, allergen_ings);
    }

    let mut identified_ingredients = HashSet::new();

    let mut out = HashMap::new();

    while identified_ingredients.len() < all_allergens.len() {
        for (allergen, candidates) in potential_ingredients.iter_mut() {
            candidates.retain(|c| !identified_ingredients.contains(c));

            if candidates.len() == 1 {
                let ingredient = candidates.iter().next().unwrap();
                out.insert(*ingredient, *allergen);
                identified_ingredients.insert(*ingredient);
            }
        }
    }

    out
}

fn parse(input: &str) -> Vec<Food> {
    input
        .lines()
        .map(|l| {
            let mut parts = l.split(" (contains ");
            let ingredients = parts.next().unwrap().split_whitespace().collect();
            let allergens = parts
                .next()
                .unwrap()
                .trim_end_matches(')')
                .split(", ")
                .collect();

            (ingredients, allergens)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2061, part1(include_str!("../input/day21.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(
            "cdqvp,dglm,zhqjs,rbpg,xvtrfz,tgmzqjz,mfqgx,rffqhl",
            &part2(include_str!("../input/day21.txt"))
        );
    }
}

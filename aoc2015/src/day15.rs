const INGREDIENTS: [(&str, i32, i32, i32, i32, i32); 4] = [
    ("Sugar", 3, 0, 0, -3, 2),
    ("Sprinkles", -3, 3, 0, 0, 9),
    ("Candy", -1, 0, 4, 0, 1),
    ("Chocolate", 0, 0, -2, 2, 8),
];

pub fn part1() -> i32 {
    scores().map(|(s, _)| s).max().unwrap()
}

pub fn part2() -> i32 {
    scores()
        .filter(|(_s, c)| *c == 500)
        .map(|(s, _)| s)
        .max()
        .unwrap()
}

fn scores() -> impl Iterator<Item = (i32, i32)> {
    proportions().map(|(i, j, k, q)| {
        let capacity = INGREDIENTS[0].1 * i
            + INGREDIENTS[1].1 * j
            + INGREDIENTS[2].1 * k
            + INGREDIENTS[3].1 * q;

        let durability = INGREDIENTS[0].2 * i
            + INGREDIENTS[1].2 * j
            + INGREDIENTS[2].2 * k
            + INGREDIENTS[3].2 * q;

        let flavor = INGREDIENTS[0].3 * i
            + INGREDIENTS[1].3 * j
            + INGREDIENTS[2].3 * k
            + INGREDIENTS[3].3 * q;

        let texture = INGREDIENTS[0].4 * i
            + INGREDIENTS[1].4 * j
            + INGREDIENTS[2].4 * k
            + INGREDIENTS[3].4 * q;

        let calories = INGREDIENTS[0].5 * i
            + INGREDIENTS[1].5 * j
            + INGREDIENTS[2].5 * k
            + INGREDIENTS[3].5 * q;

        let score = capacity.max(0) * durability.max(0) * flavor.max(0) * texture.max(0);

        (score, calories.max(0))
    })
}

fn proportions() -> impl Iterator<Item = (i32, i32, i32, i32)> {
    // manually unrolled input for simplicity
    (1..100 - 1).flat_map(move |i| {
        (1..100 - i).flat_map(move |j| (1..100 - i - j).map(move |k| (i, j, k, 100 - i - j - k)))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(222_870, part1());
    }

    #[test]
    fn solution_part2() {
        assert_eq!(117_936, part2());
    }
}

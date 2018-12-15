struct Recipes {
    scores: Vec<u8>,
    elf0_pos: usize,
    elf1_pos: usize,
}

pub fn part1(nrecipes: usize) -> Vec<u8> {
    let mut recipes = Recipes::new();

    while recipes.scores.len() < nrecipes + 10 {
        recipes.brew();
    }

    recipes.scores[nrecipes..nrecipes + 10]
        .iter()
        .cloned()
        .collect()
}

pub fn part2(end_recipe: &[u8]) -> usize {
    let mut recipes = Recipes::new();

    loop {
        let brewed_count = recipes.brew();

        for b in 0..brewed_count {
            if end_recipe.len() > recipes.scores.len() - b {
                continue;
            }

            let new_recipe = &recipes.scores
                [recipes.scores.len() - end_recipe.len() - b..recipes.scores.len() - b];

            if new_recipe == end_recipe {
                return recipes.scores.len() - end_recipe.len() - b;
            }
        }
    }
}

impl Recipes {
    fn new() -> Self {
        Recipes {
            scores: vec![3, 7],
            elf0_pos: 0,
            elf1_pos: 1,
        }
    }

    fn brew(&mut self) -> usize {
        let score = self.scores[self.elf0_pos] + self.scores[self.elf1_pos];

        let mut brewed_count = 1;
        if score >= 10 {
            self.scores.push(score / 10);
            brewed_count += 1
        }

        self.scores.push(score % 10);

        self.elf0_pos =
            (1 + self.elf0_pos + usize::from(self.scores[self.elf0_pos])) % self.scores.len();
        self.elf1_pos =
            (1 + self.elf1_pos + usize::from(self.scores[self.elf1_pos])) % self.scores.len();

        brewed_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(vec![6, 9, 8, 5, 1, 0, 3, 1, 2, 2], part1(380621));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(20182290, part2(&[3, 8, 0, 6, 2, 1]));
    }
}

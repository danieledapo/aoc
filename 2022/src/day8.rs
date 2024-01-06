pub fn part1(input: &str) -> usize {
    let grid = parse(input);

    let mut visible;
    if grid.len() == 1 {
        visible = grid[0].len();
    } else if grid.len() == 2 {
        visible = grid[0].len() * 2;
    } else {
        visible = grid.len() * 2 + grid[0].len() * 2 - 4;
    }

    for i in 1..grid.len().saturating_sub(1) {
        for j in 1..grid[i].len().saturating_sub(1) {
            let h = grid[i][j];
            let is_visible = (0..i).all(|k| grid[k][j] < h)
                || (0..j).all(|k| grid[i][k] < h)
                || (i + 1..grid.len()).all(|k| grid[k][j] < h)
                || (j + 1..grid[i].len()).all(|k| grid[i][k] < h);

            if is_visible {
                visible += 1;
            }
        }
    }

    visible
}

pub fn part2(input: &str) -> usize {
    let grid = parse(input);

    let mut max_scenic_score = 0;

    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            let h = grid[i][j];
            let score = count((0..i).rev(), |k| grid[k][j], h)
                * count((0..j).rev(), |k| grid[i][k], h)
                * count(i + 1..grid.len(), |k| grid[k][j], h)
                * count(j + 1..grid[i].len(), |k| grid[i][k], h);

            max_scenic_score = max_scenic_score.max(score);
        }
    }

    max_scenic_score
}

fn count(r: impl Iterator<Item = usize>, g: impl Fn(usize) -> u32, h: u32) -> usize {
    // no take_while_inclusive...
    let mut c = 0;
    for i in r {
        c += 1;
        if g(i) >= h {
            break;
        }
    }
    c
}

fn parse(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(1695, part1(include_str!("../input/day8.txt"),));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(287040, part2(include_str!("../input/day8.txt"),));
    }
}

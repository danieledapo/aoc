#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Cell {
    Trap,
    Safe,
}

pub fn part1(input: &str) -> usize {
    safe_cells_after(parse(input), 40)
}

pub fn part2(input: &str) -> usize {
    dump(parse(input), 400_000, "/tmp/fuffa.txt").unwrap();
    safe_cells_after(parse(input), 400_000)
}

pub fn safe_cells_after(mut cells: Vec<Cell>, n: usize) -> usize {
    let mut c = 0;

    for _ in 0..n {
        c += cells.iter().filter(|&&c| c == Cell::Safe).count();
        cells = evolve(&cells);
    }

    c
}

fn evolve(cells: &[Cell]) -> Vec<Cell> {
    use Cell::*;
    let mut new_cells = Vec::with_capacity(cells.len());

    let evolve_cell = |l, c, r| match (l, c, r) {
        (Trap, Trap, Safe) | (Safe, Trap, Trap) | (Trap, Safe, Safe) | (Safe, Safe, Trap) => Trap,
        _ => Safe,
    };

    new_cells.push(evolve_cell(Safe, cells[0], cells[1]));

    for i in 1..cells.len() - 1 {
        new_cells.push(evolve_cell(cells[i - 1], cells[i], cells[i + 1]));
    }

    new_cells.push(evolve_cell(
        cells[cells.len() - 2],
        cells[cells.len() - 1],
        Safe,
    ));

    new_cells
}

fn parse(line: &str) -> Vec<Cell> {
    line.trim()
        .chars()
        .map(|c| match c {
            '^' => Cell::Trap,
            '.' => Cell::Safe,
            _ => unreachable!(),
        })
        .collect()
}

pub fn dump(mut cells: Vec<Cell>, iterations: usize, filename: &str) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::Write;

    let mut f = File::create(filename)?;

    let mut dump_cells = |cells: &[Cell]| {
        writeln!(
            f,
            "{}",
            cells
                .iter()
                .map(|c| {
                    match c {
                        Cell::Safe => '.',
                        Cell::Trap => '^',
                    }
                })
                .collect::<String>()
        )
    };

    dump_cells(&cells)?;
    for _ in 0..iterations {
        cells = evolve(&cells);
        dump_cells(&cells)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day18.txt")), 2013);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day18.txt")), 20006289);
    }
}

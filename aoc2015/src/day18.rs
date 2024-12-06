#[derive(Debug, Clone, PartialEq, Eq)]
enum Light {
    On,
    Off,
}

pub fn part1(input: &str) -> usize {
    let mut automata = parse_automata(input);

    for _ in 0..100 {
        automata = evolve(&automata);
    }

    automata
        .iter()
        .flat_map(|r| r.iter())
        .filter(|l| **l == Light::On)
        .count()
}

pub fn part2(input: &str) -> usize {
    let mut automata = parse_automata(input);

    let set_corners = |auto: &mut [Vec<Light>]| {
        auto[0][0] = Light::On;
        auto[0][99] = Light::On;
        auto[99][0] = Light::On;
        auto[99][99] = Light::On;
    };

    set_corners(&mut automata);

    for _ in 0..100 {
        automata = evolve(&automata);
        set_corners(&mut automata);
    }

    automata
        .iter()
        .flat_map(|r| r.iter())
        .filter(|l| **l == Light::On)
        .count()
}

fn evolve(old: &[Vec<Light>]) -> Vec<Vec<Light>> {
    (0..old.len())
        .map(|y| {
            (0..old[y].len())
                .map(|x| {
                    let neighbors = alive_neighbors(&old, x, y);

                    match old[y][x] {
                        Light::On => {
                            if neighbors == 2 || neighbors == 3 {
                                Light::On
                            } else {
                                Light::Off
                            }
                        }
                        Light::Off => {
                            if neighbors == 3 {
                                Light::On
                            } else {
                                Light::Off
                            }
                        }
                    }
                })
                .collect()
        })
        .collect()
}

fn alive_neighbors(gen: &[Vec<Light>], x: usize, y: usize) -> usize {
    let mut count = 0;

    if y > 0 && x > 0 && gen[y - 1][x - 1] == Light::On {
        count += 1;
    }

    if y > 0 && gen[y - 1][x] == Light::On {
        count += 1;
    }

    if y > 0 && x < gen[y - 1].len() - 1 && gen[y - 1][x + 1] == Light::On {
        count += 1;
    }

    if x > 0 && gen[y][x - 1] == Light::On {
        count += 1;
    }

    if x < gen[y].len() - 1 && gen[y][x + 1] == Light::On {
        count += 1;
    }

    if y < gen.len() - 1 && x > 0 && gen[y + 1][x - 1] == Light::On {
        count += 1;
    }

    if y < gen.len() - 1 && gen[y + 1][x] == Light::On {
        count += 1;
    }

    if y < gen.len() - 1 && x < gen[y + 1].len() - 1 && gen[y + 1][x + 1] == Light::On {
        count += 1;
    }

    count
}

fn parse_automata(input: &str) -> Vec<Vec<Light>> {
    input
        .trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '#' => Light::On,
                    '.' => Light::Off,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(814, part1(include_str!("../input/day18.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(924, part2(include_str!("../input/day18.txt")));
    }
}

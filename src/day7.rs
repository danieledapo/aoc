use crate::day5;

pub fn part1(input: &str) -> i32 {
    let prog = day5::Machine::new(input);

    permutations(&[0, 1, 2, 3, 4])
        .map(|phases| {
            let mut input_signal = 0;

            for phase in phases {
                let mut prog = prog.clone();
                let outputs = prog.run(&mut vec![phase, input_signal]);
                input_signal = *outputs.last().unwrap();
            }

            input_signal
        })
        .max()
        .unwrap()
}

pub fn part2(input: &str) -> i32 {
    let prog = day5::Machine::with_run_mode(input, day5::RunMode::YieldOutput);

    permutations(&[5, 6, 7, 8, 9])
        .map(|phases| {
            let mut progs = vec![prog.clone(); phases.len()];
            let mut inputs = Vec::with_capacity(phases.len());
            for phase in phases {
                inputs.push(vec![phase]);
            }
            inputs[0].push(0);

            let mut last_signal = 0;
            loop {
                for i in 0..progs.len() {
                    let outputs = progs[i].run(&mut inputs[i]);
                    if outputs.is_empty() {
                        return last_signal;
                    }

                    assert!(outputs.len() == 1);
                    last_signal = *outputs.last().unwrap();
                    inputs[(i + 1) % progs.len()].extend(&outputs);
                }
            }
        })
        .max()
        .unwrap()
}

fn permutations<T: Clone>(elems: &[T]) -> impl Iterator<Item = Vec<T>> {
    use std::iter;

    let n = elems.len();
    let mut c = vec![0; n];
    let mut i = 0;
    let mut v = elems.to_vec();

    iter::once(v.clone()).chain(iter::from_fn(move || loop {
        if i >= n {
            return None;
        }

        if c[i] < i {
            if i % 2 == 0 {
                v.swap(0, i);
            } else {
                v.swap(c[i], i);
            }
            c[i] += 1;
            i = 0;
            break Some(v.clone());
        } else {
            c[i] = 0;
            i += 1;
        }
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day7.txt")), 20413);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day7.txt")), 3321777);
    }
}

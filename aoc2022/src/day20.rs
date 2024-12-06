pub fn part1(input: &str) -> i64 {
    go(input, 1, 1)
}

pub fn part2(input: &str) -> i64 {
    go(input, 811589153, 10)
}

fn go(input: &str, key: i64, iterations: usize) -> i64 {
    let (mut order, mut ix) = parse(input);
    for n in &mut order {
        *n *= key;
    }

    for _ in 0..iterations {
        for (i, &n) in order.iter().enumerate() {
            if n == 0 {
                continue;
            }

            let p = ix[i];
            let np = (p as i64 + n).rem_euclid(order.len() as i64 - 1) as usize;

            for j in &mut ix {
                if *j > p {
                    *j -= 1;
                }

                if *j >= np {
                    *j = (*j + 1) % order.len();
                }
            }
            ix[i] = np;
        }
    }

    let mut test: Vec<_> = vec![0; order.len()];
    for i in 0..order.len() {
        test[ix[i]] = order[i];
    }

    let p0 = test.iter().position(|n| *n == 0).unwrap();
    [1000, 2000, 3000]
        .into_iter()
        .map(|d| test[(p0 + d) % order.len()])
        .sum()
}

fn parse(input: &str) -> (Vec<i64>, Vec<usize>) {
    let v: Vec<_> = input.lines().map(|l| l.parse().unwrap()).collect();
    let ix = (0..v.len()).collect();

    (v, ix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(3466, part1(include_str!("../input/day20.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(9995532008348, part2(include_str!("../input/day20.txt")));
    }
}

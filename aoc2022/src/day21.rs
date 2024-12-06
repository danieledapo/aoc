use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Formula {
    N(i128),
    F {
        l: String,
        r: String,
        op: fn(i128, i128) -> i128,
    },
}

pub fn part1(input: &str) -> i128 {
    go(&parse(input), "root")
}

pub fn part2(input: &str) -> i128 {
    let mut tree = parse(input);
    let (l, r) = match &tree["root"] {
        Formula::N(_) => unreachable!(),
        Formula::F { l, r, .. } => (l.to_string(), r.to_string()),
    };

    let l1 = go(&tree, &l);
    tree.insert("humn".to_string(), Formula::N(500));

    let (node, target) = if go(&tree, &l) == l1 {
        (r, l1)
    } else {
        (l, go(&tree, &r))
    };

    dbg!(target);

    let mut lo = 0;
    let mut high = i128::from(i64::MAX) + 1;
    while lo < high {
        let i = (lo + high) / 2;

        tree.insert("humn".to_string(), Formula::N(i));

        let score = target - go(&tree, &node);
        if score == 0 {
            dbg!(lo, high);
            return i;
        }

        if score < 0 {
            lo = i;
        } else {
            high = i;
        }
    }

    unreachable!()
}

fn go(tree: &HashMap<String, Formula>, r: &str) -> i128 {
    match &tree[r] {
        Formula::N(n) => *n,
        Formula::F { l, r, op } => {
            let a = go(tree, l);
            let b = go(tree, r);

            (op)(a, b)
        }
    }
}

fn parse(input: &str) -> HashMap<String, Formula> {
    input
        .lines()
        .map(|l| {
            let (n, fs) = l.split_once(": ").unwrap();

            let f = if let Ok(nn) = fs.parse::<i128>() {
                Formula::N(nn)
            } else {
                let mut parts = fs.split_whitespace();
                let l = parts.next().unwrap().to_string();
                let op = match parts.next().unwrap() {
                    "+" => |a, b| a + b,
                    "-" => |a, b| a - b,
                    "*" => |a, b| a * b,
                    "/" => |a, b| a / b,
                    _ => unreachable!(),
                };
                let r = parts.next().unwrap().to_string();

                Formula::F { l, r, op }
            };

            (n.to_string(), f)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(282285213953670, part1(include_str!("../input/day21.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(3699945358564, part2(include_str!("../input/day21.txt")));
    }
}

use std::cmp::Ordering;

use json::json;
use serde_json as json;

pub fn part1(input: &str) -> usize {
    let mut s = 0;

    for (i, (a, b)) in parse(input).into_iter().enumerate() {
        if value_cmp(&a, &b) == Ordering::Less {
            s += i + 1;
        }
    }

    s
}

pub fn part2(input: &str) -> usize {
    let mut lines = vec![];
    for (a, b) in parse(input) {
        lines.push(a);
        lines.push(b);
    }

    lines.push(json!([[2]]));
    lines.push(json!([[6]]));

    lines.sort_unstable_by(value_cmp);

    let ax = lines
        .binary_search_by(|a| value_cmp(a, &json!([[2]])))
        .unwrap();
    let bx = lines
        .binary_search_by(|a| value_cmp(a, &json!([[6]])))
        .unwrap();

    (ax + 1) * (bx + 1)
}

fn value_cmp(a: &json::Value, b: &json::Value) -> std::cmp::Ordering {
    use json::Value::*;
    use std::cmp::Ordering::*;

    match (a, b) {
        (Number(n), Number(m)) => n.as_i64().unwrap().cmp(&m.as_i64().unwrap()),

        (Array(_), Number(n)) => value_cmp(a, &json!([n])),
        (Number(n), Array(_)) => value_cmp(&json!([n]), b),

        (Array(vs), Array(xs)) => {
            let l = usize::max(vs.len(), xs.len());

            for i in 0..l {
                match (vs.get(i), xs.get(i)) {
                    (None, None) => unreachable!(),
                    (None, Some(_)) => return Less,
                    (Some(_), None) => return Greater,
                    (Some(a), Some(b)) => {
                        let o = value_cmp(a, b);
                        if o != Equal {
                            return o;
                        }
                    }
                }
            }

            Equal
        }

        _ => unreachable!(),
    }
}

fn parse(input: &str) -> Vec<(json::Value, json::Value)> {
    let mut res = vec![];

    let mut lines = input.lines().peekable();
    while lines.peek().is_some() {
        let a = json::from_str(lines.next().unwrap()).unwrap();
        let b = json::from_str(lines.next().unwrap()).unwrap();
        lines.next();
        res.push((a, b));
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(5588, part1(include_str!("../input/day13.txt")));
    }

    #[test]
    pub fn test_part2() {
        assert_eq!(23958, part2(include_str!("../input/day13.txt")));
    }
}

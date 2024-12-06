pub fn part1(input: &str) -> f64 {
    let v: serde_json::Value = serde_json::from_str(input).unwrap();

    fn sum(v: &serde_json::Value) -> f64 {
        use serde_json::Value;

        match v {
            Value::Number(n) => n.as_f64().unwrap(),
            Value::Array(a) => a.iter().map(sum).sum(),
            Value::Object(o) => o.values().map(sum).sum(),
            _ => 0.0,
        }
    }

    sum(&v)
}

pub fn part2(input: &str) -> f64 {
    let v: serde_json::Value = serde_json::from_str(input).unwrap();

    fn sum(v: &serde_json::Value) -> f64 {
        use serde_json::Value;

        match v {
            Value::Number(n) => n.as_f64().unwrap(),
            Value::Array(a) => a.iter().map(sum).sum(),
            Value::Object(o) => {
                let has_red = o.values().any(|v| {
                    if let Value::String(s) = v {
                        s == "red"
                    } else {
                        false
                    }
                });

                if !has_red {
                    o.values().map(sum).sum()
                } else {
                    0.0
                }
            }
            _ => 0.0,
        }
    }

    sum(&v)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(156_366, part1(include_str!("../input/day12.txt")) as u64);
    }

    #[test]
    fn solution_part2() {
        assert_eq!(96852, part2(include_str!("../input/day12.txt")) as u64);
    }
}

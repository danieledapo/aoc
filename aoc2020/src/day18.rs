#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok {
    Add,
    Mul,
    Open,
    Num(u64),
}

pub fn part1(input: &str) -> u64 {
    input.lines().map(|l| eval(l, false)).sum()
}

pub fn part2(input: &str) -> u64 {
    input.lines().map(|l| eval(l, true)).sum()
}

fn eval(input: &str, priority_add: bool) -> u64 {
    let toks = infix_to_rpn(input, priority_add);

    let mut out = vec![];
    for tok in toks {
        match tok {
            Tok::Open => unreachable!(),
            Tok::Num(n) => out.push(n),
            Tok::Add => {
                let a = out.pop().unwrap();
                let b = out.pop().unwrap();
                out.push(a + b);
            }
            Tok::Mul => {
                let a = out.pop().unwrap();
                let b = out.pop().unwrap();
                out.push(a * b);
            }
        }
    }

    out.pop().unwrap()
}

fn infix_to_rpn(input: &str, priority_add: bool) -> Vec<Tok> {
    let mut toks = vec![];
    let mut ops = vec![];

    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if c.is_whitespace() {
            continue;
        }

        if c.is_digit(10) {
            let mut n = c.to_digit(10).unwrap();
            while let Some(c) = chars.peek() {
                if !c.is_digit(10) {
                    break;
                }
                n = n * 10 + c.to_digit(10).unwrap();
                chars.next().unwrap();
            }
            toks.push(Tok::Num(n.into()));
            continue;
        }

        if c == '(' {
            ops.push(Tok::Open);
            continue;
        }

        if c == ')' {
            loop {
                let op = ops.pop().unwrap();
                if op == Tok::Open {
                    break;
                }
                toks.push(op);
            }
            continue;
        }

        while let Some(&op) = ops.last() {
            let has_predecence = priority_add && (op == Tok::Mul && c == '+');
            if op == Tok::Open || has_predecence {
                break;
            }
            toks.push(op);
            ops.pop().unwrap();
        }

        ops.push(match c {
            '+' => Tok::Add,
            '*' => Tok::Mul,
            _ => unreachable!(),
        });
    }
    toks.extend(ops.iter().rev());

    toks
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(3885386961962, part1(include_str!("../input/day18.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(112899558798666, part2(include_str!("../input/day18.txt")));
    }
}

pub fn part1(start: Vec<u8>) -> usize {
    (0..40).fold(start, |s, _| advance(&s)).len()
}

pub fn part2(start: Vec<u8>) -> usize {
    (0..50).fold(start, |s, _| advance(&s)).len()
}

fn advance(n: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(n.len());

    let mut i = 0;
    while i < n.len() {
        let d = n[i];
        let mut c = 1;

        i += 1;

        while i < n.len() && n[i] == d {
            c += 1;
            i += 1;
        }

        let tmp = out.len();
        while c > 0 {
            out.push(c % 10);
            c /= 10;
        }
        (&mut out[tmp..]).reverse();
        out.push(d);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(360_154, part1(vec![1, 1, 1, 3, 1, 2, 2, 1, 1, 3]));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(5_103_798, part2(vec![1, 1, 1, 3, 1, 2, 2, 1, 1, 3]));
    }
}

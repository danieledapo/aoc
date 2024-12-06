pub fn part1(input: &str) -> usize {
    input
        .lines()
        .map(get_triangle)
        .filter(|t| valid(*t))
        .count()
}

pub fn part2(input: &str) -> usize {
    let mut lines = input.trim().lines();

    let mut c = 0;
    while let Some(l) = lines.next() {
        let t1 = get_triangle(l);
        let t2 = get_triangle(lines.next().unwrap());
        let t3 = get_triangle(lines.next().unwrap());

        if valid([t1[0], t2[0], t3[0]]) {
            c += 1;
        }
        if valid([t1[1], t2[1], t3[1]]) {
            c += 1;
        }
        if valid([t1[2], t2[2], t3[2]]) {
            c += 1;
        }
    }

    c
}

fn valid([a, b, c]: [i32; 3]) -> bool {
    (a + b > c) && (a + c > b) && (b + c > a)
}

fn get_triangle(l: &str) -> [i32; 3] {
    let mut parts = l.trim().split_whitespace();
    let a: i32 = parts.next().unwrap().parse().unwrap();
    let b: i32 = parts.next().unwrap().parse().unwrap();
    let c: i32 = parts.next().unwrap().parse().unwrap();

    [a, b, c]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day3.txt")), 869);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day3.txt")), 1544);
    }
}

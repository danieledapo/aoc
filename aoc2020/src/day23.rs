pub fn part1(input: &str) -> String {
    let cups = parse(input);
    let c = game(&cups, 100);

    let mut p1 = String::new();
    let mut i = 1;
    while c[i] != 1 {
        p1.push_str(&c[i].to_string());
        i = c[i];
    }
    p1
}

pub fn part2(input: &str) -> usize {
    let mut cups = parse(input);
    cups.extend(10..=1000000);

    let c = game(&cups, 10000000);
    c[1] * c[c[1]]
}

fn game(cups: &[usize], moves: usize) -> Vec<usize> {
    let min = *cups.iter().min().unwrap();
    let max = *cups.iter().max().unwrap();

    let mut index = vec![0; max + 1];
    for i in 0..cups.len() {
        index[cups[i]] = cups[(i + 1) % cups.len()];
    }

    let mut cur_cup = cups[0];
    for _ in 0..moves {
        let p1 = index[cur_cup];
        let p2 = index[p1];
        let p3 = index[p2];

        index[cur_cup] = index[p3];

        let mut dest_cup = if cur_cup > min { cur_cup - 1 } else { max };
        while [p1, p2, p3].contains(&dest_cup) || dest_cup < min || dest_cup > max {
            dest_cup = if dest_cup > min { dest_cup - 1 } else { max };
        }

        let tmp = index[dest_cup];
        index[dest_cup] = p1;
        index[p1] = p2;
        index[p2] = p3;
        index[p3] = tmp;

        cur_cup = index[cur_cup];
    }

    index
}

fn parse(input: &str) -> Vec<usize> {
    input
        .chars()
        .map(|d| d.to_digit(10).unwrap() as usize)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!("47598263", &part1("123487596"));
    }

    #[test]
    fn test_part2() {
        assert_eq!(248009574232, part2("123487596"));
    }
}

pub fn part1(seed: &str, space: usize) -> String {
    let mut data = seed.to_string();
    while data.len() < space {
        data = dragon_curve(data);
    }

    data = data[..space].to_string();

    loop {
        let cksum = checksum(&data);
        if cksum.len() % 2 == 1 {
            return cksum;
        }

        data = cksum;
    }
}

fn checksum(data: &str) -> String {
    data.as_bytes()
        .chunks_exact(2)
        .map(|chunk| if chunk[0] == chunk[1] { '1' } else { '0' })
        .collect::<String>()
}

fn dragon_curve(state: String) -> String {
    state
        .chars()
        .chain(Some('0'))
        .chain(state.chars().rev().map(|c| match c {
            '0' => '1',
            '1' => '0',
            _ => unreachable!(),
        }))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("11100010111110100", 272), "10100011010101011");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part1("11100010111110100", 35651584), "01010001101011001");
    }
}

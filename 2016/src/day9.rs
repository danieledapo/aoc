enum Version {
    V1,
    V2,
}

pub fn part1(input: &str) -> usize {
    decompressed_len(input, Version::V1)
}

pub fn part2(input: &str) -> usize {
    decompressed_len(input, Version::V2)
}

fn decompressed_len(input: &str, version: Version) -> usize {
    let mut total_len = 0;

    let mut chars = input.trim().chars();
    let chars = chars.by_ref();
    while let Some(ch) = chars.next() {
        if ch != '(' {
            total_len += 1;
            continue;
        }

        let marker = chars.take_while(|c| *c != ')').collect::<String>();

        let mut parts = marker.split('x');
        let len: usize = parts.next().unwrap().parse().unwrap();
        let repeat: usize = parts.next().unwrap().parse().unwrap();

        let sub_len = match version {
            Version::V1 => {
                chars.take(len).last();
                len
            }
            Version::V2 => {
                let sub = chars.take(len).collect::<String>();
                decompressed_len(&sub, Version::V2)
            }
        };

        total_len += sub_len * repeat;
    }

    total_len
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day9.txt")), 70186);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day9.txt")), 10915059201);
    }
}

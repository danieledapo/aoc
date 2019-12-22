pub fn part1(door_id: &str) -> String {
    let mut res = String::new();

    for i in 0.. {
        let hash = md5::compute(format!("{}{}", door_id, i));
        let hash = format!("{:x}", hash);
        if hash.starts_with(&"00000") {
            res.push(hash.chars().nth(5).unwrap());

            if res.len() >= 8 {
                break;
            }
        }
    }

    res
}

pub fn part2(door_id: &str) -> [char; 8] {
    let mut res = [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '];
    let mut count = 0;

    for i in 0.. {
        let hash = md5::compute(format!("{}{}", door_id, i));
        let hash = format!("{:x}", hash);

        if hash.starts_with(&"00000") {
            let ix = usize::from_str_radix(&hash[5..6], 16).unwrap();
            let c = hash.chars().nth(6).unwrap();

            if let Some(m) = res.get_mut(ix) {
                if *m != ' ' {
                    continue;
                }

                *m = c;

                count += 1;
                if count >= 8 {
                    break;
                }
            }
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("ugkcyxxp"), "d4cd2ee1");
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("ugkcyxxp"), ['f', '2', 'c', '7', '3', '0', 'e', '5']);
    }
}

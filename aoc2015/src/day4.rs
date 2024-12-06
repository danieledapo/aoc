pub fn mine(prefix: &str, secret: &str) -> usize {
    for i in 0.. {
        let digest = md5::compute(&format!("{}{}", secret, i));
        let hex_digest = format!("{:x}", digest);

        if hex_digest.starts_with(&prefix) {
            return i;
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(117_946, mine("00000", "ckczppom"));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(3_938_038, mine("000000", "ckczppom"));
    }
}

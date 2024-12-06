pub fn part1(card_pubkey: u128, door_pubkey: u128) -> u128 {
    // let card_loop_size = find_loop_size(7, card_pubkey);
    let door_loop_size = find_loop_size(7, door_pubkey);

    transform(card_pubkey, door_loop_size)
}

fn transform(subject_number: u128, n: u128) -> u128 {
    let mut v = 1;
    for _ in 0..n {
        v = (v * subject_number) % 20201227;
    }
    v
}

fn find_loop_size(subject_number: u128, target: u128) -> u128 {
    let mut v = 1;
    for l in 0.. {
        if v == target {
            return l;
        }

        v = (v * subject_number) % 20201227;
    }

    panic!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(354320, part1(12232269, 19452773));
    }
}

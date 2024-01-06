use md5::{Digest, Md5};

pub fn part1(salt: &str) -> usize {
    run(salt, 0)
}

pub fn part2(salt: &str) -> usize {
    run(salt, 2016)
}

fn calc_hash(s: &str, n: usize) -> String {
    (0..n).fold(format!("{:x}", Md5::digest(s.as_bytes())), |hash, _| {
        format!("{:x}", Md5::digest(hash.as_bytes()))
    })
}

fn run(salt: &str, initial_hashes: usize) -> usize {
    let mut key = 0;

    let mut hashes = (0..=1_000)
        .map(|i| calc_hash(&format!("{}{}", salt, i), initial_hashes))
        .collect::<Vec<_>>();

    for cur_candidate in 0.. {
        hashes.push(calc_hash(
            &format!("{}{}", salt, cur_candidate + 1 + 1_000),
            initial_hashes,
        ));

        let hash = &hashes[cur_candidate];
        let c = {
            let hash = hash.as_bytes();
            let mut c = None;
            for i in 0..hash.len().saturating_sub(2) {
                if hash[i] == hash[i + 1] && hash[i] == hash[i + 2] {
                    c = Some(hash[i]);
                    break;
                }
            }

            c
        };

        if let Some(c) = c {
            let ccccc = (0..5).map(|_| c as char).collect::<String>();
            let has_5 = hashes[cur_candidate + 1..cur_candidate + 1 + 1_000]
                .iter()
                .any(|h| h.contains(&ccccc));

            if has_5 {
                key += 1;

                if key == 64 {
                    return cur_candidate;
                }
            }
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("qzyelonm"), 15168);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("qzyelonm"), 20864);
    }
}

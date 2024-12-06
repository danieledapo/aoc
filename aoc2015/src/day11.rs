use std::collections::HashMap;

pub fn generate(pwd: &[u8]) -> Vec<u8> {
    let mut new_pwd = pwd.to_vec();
    next_pwd(&mut new_pwd);

    while !is_valid(&new_pwd) {
        next_pwd(&mut new_pwd);
    }

    new_pwd
}

fn next_pwd(pwd: &mut [u8]) {
    let mut i = pwd.len() - 1;

    loop {
        pwd[i] += 1;

        if pwd[i] > b'z' {
            pwd[i] = b'a';

            if i == 0 {
                i = pwd.len() - 1;
            } else {
                i -= 1;
            }
        } else {
            break;
        }
    }
}

fn is_valid(pwd: &[u8]) -> bool {
    has_increasing_streak(pwd) && !has_blacklisted_letters(pwd) && has_2_pairs(pwd)
}

fn has_increasing_streak(pwd: &[u8]) -> bool {
    pwd.windows(3).any(|c| c[0] + 1 == c[1] && c[1] + 1 == c[2])
}

fn has_blacklisted_letters(pwd: &[u8]) -> bool {
    pwd.iter().any(|c| *c == b'i' || *c == b'o' || *c == b'l')
}

fn has_2_pairs(pwd: &[u8]) -> bool {
    let mut pairs: HashMap<&[u8], usize> = HashMap::new();

    for p in pwd.windows(2) {
        if p[0] == p[1] {
            *pairs.entry(p).or_default() += 1;
        }
    }

    pairs.values().filter(|c| **c >= 1).count() >= 2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(b"vzbxxyzz".to_vec(), generate(b"vzbxkghb"));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(b"vzcaabcc".to_vec(), generate(b"vzbxxyzz"));
    }
}

use std::collections::HashMap;

use once_cell::sync::OnceCell;
use regex::Regex;

pub fn part1(input: &str) -> usize {
    let mut c = 0;

    for_each_passport(input, |fields| {
        let to_check = [
            "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid",
            // "cid"
        ];

        if to_check.iter().all(|f| fields.get(f).is_some()) {
            c += 1;
        }
    });

    c
}

pub fn part2(input: &str) -> usize {
    let mut c = 0;

    let in_range = |v: i32, min: i32, max: i32| {
        if (min..=max).contains(&v) {
            Some(())
        } else {
            None
        }
    };
    let valid_year = |yr: &&str, min, max| {
        if yr.len() != 4 {
            return None;
        }
        in_range(yr.parse::<i32>().ok()?, min, max)
    };

    let valid_height = |h: &&str| {
        if let Some(h) = h.strip_suffix("cm") {
            return in_range(h.parse::<i32>().ok()?, 150, 193);
        }
        if let Some(h) = h.strip_suffix("in") {
            return in_range(h.parse::<i32>().ok()?, 59, 76);
        }
        None
    };

    let valid_hair_color = |h: &&str| {
        let h = h.strip_prefix('#')?;
        if h.len() != 6 || h.chars().any(|c| !c.is_ascii_hexdigit()) {
            return None;
        }

        Some(())
    };

    let valid_eye_color = |e: &&str| {
        ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]
            .binary_search(e)
            .ok()
    };

    let valid_passport_id = |p: &&str| {
        if p.len() != 9 {
            return None;
        }
        p.parse::<u32>().ok()
    };

    for_each_passport(input, |fields| {
        let ok = Some(())
            .and_then(|_| valid_year(fields.get("byr")?, 1920, 2002))
            .and_then(|_| valid_year(fields.get("iyr")?, 2010, 2020))
            .and_then(|_| valid_year(fields.get("eyr")?, 2020, 2030))
            .and_then(|_| valid_height(fields.get("hgt")?))
            .and_then(|_| valid_hair_color(fields.get("hcl")?))
            .and_then(|_| valid_eye_color(fields.get("ecl")?))
            .and_then(|_| valid_passport_id(fields.get("pid")?))
            .is_some();

        if ok {
            c += 1;
        }
    });

    c
}

fn for_each_passport(input: &str, mut fun: impl FnMut(&HashMap<&str, &str>)) {
    static RE: OnceCell<Regex> = OnceCell::new();

    let re = RE.get_or_init(|| Regex::new(r"(\S+):(\S+)").unwrap());

    let mut fields = HashMap::new();
    for l in input.lines() {
        if l.is_empty() {
            fun(&fields);
            fields.clear();
            continue;
        }

        for capture in re.captures_iter(&l) {
            let f = capture.get(1).unwrap().as_str();
            let v = capture.get(2).unwrap().as_str();
            fields.insert(f, v);
        }
    }

    if !fields.is_empty() {
        fun(&fields);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(200, part1(include_str!("../input/day4.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(116, part2(include_str!("../input/day4.txt")));
    }
}

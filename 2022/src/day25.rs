pub fn part1(input: &str) -> String {
    let mut s = 0;

    for l in input.lines() {
        let mut n = 0;
        for (i, d) in l.chars().rev().enumerate() {
            let s = match d {
                '1' => 1,
                '0' => 0,
                '2' => 2,
                '-' => -1,
                '=' => -2,
                _ => unreachable!(),
            };
            n += s * 5_i64.pow(i as u32);
        }

        s += n;
    }

    let mut res = vec![];
    while s > 0 {
        match s % 5 {
            0 => res.push(b'0'),
            1 => res.push(b'1'),
            2 => res.push(b'2'),
            3 => {
                res.push(b'=');
                s += 4;
            }
            4 => {
                res.push(b'-');
                s += 5;
            }
            _ => unreachable!(),
        }

        s /= 5;
    }

    res.reverse();
    String::from_utf8(res).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_part1() {
        assert_eq!(
            "2=-1=0",
            &part1(
                r#"1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"#
            )
        );

        assert_eq!(
            "20=212=1-12=200=00-1",
            &part1(include_str!("../input/day25.txt"))
        );
    }
}

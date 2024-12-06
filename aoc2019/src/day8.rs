pub fn part1(inp: &[u8], (w, h): (usize, usize)) -> u32 {
    inp.chunks(w * h)
        .filter(|c| c.len() == w * h)
        .map(|c| {
            c.iter().fold((0, 0, 0), |(n0, n1, n2), n| match n {
                b'0' => (n0 + 1, n1, n2),
                b'1' => (n0, n1 + 1, n2),
                b'2' => (n0, n1, n2 + 1),
                _ => unreachable!(),
            })
        })
        .min_by_key(|(n0, _, _)| *n0)
        .map(|(_, n1, n2)| n1 * n2)
        .unwrap()
}

pub fn part2(inp: &[u8], (w, h): (usize, usize)) -> String {
    let mut canvas = vec![vec![0; w]; h];

    for chunk in inp.chunks(w * h).filter(|c| c.len() == w * h).rev() {
        for (i, p) in chunk.iter().enumerate() {
            let x = i % w;
            let y = i / w;
            match p {
                b'0' => canvas[y][x] = b' ',
                b'1' => canvas[y][x] = b'*',
                b'2' => {}
                _ => unreachable!(),
            }
        }
    }

    String::from_utf8(canvas.join(&b'\n')).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_bytes!("../input/day8.txt"), (25, 6)), 2375);
    }

    #[test]
    fn test_part2() {
        let msg = part2(include_bytes!("../input/day8.txt"), (25, 6));
        assert_eq!(
            msg,
            r#"
***  *  * *  * ***  *   *
*  * * *  *  * *  * *   *
*  * **   **** *  *  * * 
***  * *  *  * ***    *  
* *  * *  *  * * *    *  
*  * *  * *  * *  *   *  "#
                .trim_start()
        );
    }
}

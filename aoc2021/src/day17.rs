pub fn part1(x0: i64, x1: i64, y0: i64, y1: i64) -> i64 {
    go(x0, x1, y0, y1).0
}

pub fn part2(x0: i64, x1: i64, y0: i64, y1: i64) -> usize {
    go(x0, x1, y0, y1).1
}

fn go(x0: i64, x1: i64, y0: i64, y1: i64) -> (i64, usize) {
    let mut highest = i64::min_value();
    let mut count = 0;

    for vy in y0..1000 {
        for vx in 1..=x1 {
            let (mut vx, mut vy) = (vx, vy);
            let mut h = i64::min_value();
            let mut x = 0;
            let mut y = 0;

            loop {
                x += vx;
                y += vy;

                h = y.max(h);

                if (x0..=x1).contains(&x) && (y0..=y1).contains(&y) {
                    highest = highest.max(h);
                    count += 1;
                    break;
                }

                vx -= vx.signum();
                vy -= 1;

                if vx == 0 && x < x0 {
                    break;
                }

                if x > x1 || y < y0 {
                    break;
                }
            }
        }
    }

    (highest, count)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(4095, part1(244, 303, -91, -54));
    }

    #[test]
    fn test_part2() {
        assert_eq!(3773, part2(244, 303, -91, -54));
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Disc {
    period: usize,
    position: usize,
}

pub fn run(discs: &[Disc]) -> usize {
    for t in 0.. {
        let mut aligned = true;
        for (i, d) in discs.iter().enumerate() {
            let p = (d.position + t + 1 + i) % d.period;

            if p != 0 {
                aligned = false;
                break;
            }
        }

        if aligned {
            return t;
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(
            run(&[
                Disc {
                    period: 13,
                    position: 1
                },
                Disc {
                    period: 19,
                    position: 10
                },
                Disc {
                    period: 3,
                    position: 2
                },
                Disc {
                    period: 7,
                    position: 1
                },
                Disc {
                    period: 5,
                    position: 3
                },
                Disc {
                    period: 17,
                    position: 5
                },
            ]),
            376777
        );
    }
    #[test]
    fn test_part2() {
        assert_eq!(
            run(&[
                Disc {
                    period: 13,
                    position: 1
                },
                Disc {
                    period: 19,
                    position: 10
                },
                Disc {
                    period: 3,
                    position: 2
                },
                Disc {
                    period: 7,
                    position: 1
                },
                Disc {
                    period: 5,
                    position: 3
                },
                Disc {
                    period: 17,
                    position: 5
                },
                Disc {
                    period: 11,
                    position: 0
                },
            ]),
            3903937
        );
    }
}

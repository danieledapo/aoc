use std::collections::HashSet;


#[derive(Debug, Clone)]
pub enum Fold {
    Vertical(i32),
    Horizontal(i32),
}


#[derive(Debug)]
pub struct Origami {
    pub points: HashSet<(i32, i32)>,
    pub folds: Vec<Fold>
}


pub fn part1(input: &str) -> usize {
    let mut origami = parse(input);

    origami.apply(origami.folds[0].clone());
    origami.points.len()
}

impl Origami {
    pub fn apply(&mut self, fold: Fold) {
        let mut new_points = HashSet::new();
        match fold {
            Fold::Vertical(ax) => {
                for &(x,y) in &self.points {
                    if x < ax {
                        new_points.insert((x, y));
                        continue;
                    }

                    let x = ax - (x - ax);
                    new_points.insert((x, y));
                }
            }
            Fold::Horizontal(ay) => {
                for &(x,y) in &self.points {
                    if y < ay {
                        new_points.insert((x, y));
                        continue;
                    }

                    let y = ay - (y - ay);
                    new_points.insert((x, y));
                }
            }
        }
        self.points = new_points;
    }
}

pub fn parse(input: &str) -> Origami {
    let mut origami = Origami { points: HashSet::new(), folds: vec![]};

    let mut lines = input.lines();

    for l in lines.by_ref() {
        if l.trim().is_empty() {
            break;
        }

        let (x, y) = l.split_once(',').unwrap();
        origami.points.insert((x.parse().unwrap(), y.parse().unwrap()));
    }

    for l in lines {
        let l = l.strip_prefix("fold along ").unwrap();
        let (c, n) = l.split_once("=").unwrap();
        let n = n.parse().unwrap();
        let fold = match c {
            "x" => Fold::Vertical(n),
            "y" => Fold::Horizontal(n),
            _ => unreachable!()
        };
        origami.folds.push(fold);
    }

    origami

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(607, part1(include_str!("../input/day13.txt")));
    }
}
fn main() {
    let mut origami = aoc2021::day13::parse(include_str!("../input/day13.txt"));

    for fold in origami.folds.clone() {
        origami.apply(fold);
    }

    let max_x = origami.points.iter().map(|p| p.0).max().unwrap();
    let max_y = origami.points.iter().map(|p| p.1).max().unwrap();

    for r in 0..=max_y {
        for c in 0..=max_x {
            if origami.points.contains(&(c,r)) {
                print!("*");
            } else {
                print!(" ");
            }
        }
        println!();
    }
}

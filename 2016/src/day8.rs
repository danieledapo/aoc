#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    Rect(usize, usize),
    RotCol(usize, usize),
    RotRow(usize, usize),
}

const CANVAS_WIDTH: usize = 50;
const CANVAS_HEIGHT: usize = 6;
type Canvas = [[bool; CANVAS_WIDTH]; CANVAS_HEIGHT];

pub fn part1(input: &str) -> usize {
    let canvas = create_canvas(parse(input));

    canvas
        .iter()
        .flat_map(|l| l.iter())
        .filter(|on| **on)
        .count()
}

pub fn part2(input: &str) -> Vec<String> {
    let canvas = create_canvas(parse(input));

    canvas
        .iter()
        .map(|row| row.iter().map(|pix| if *pix { '*' } else { ' ' }).collect())
        .collect()
}

fn create_canvas(commands: impl IntoIterator<Item = Command>) -> Canvas {
    let mut canvas = [[false; CANVAS_WIDTH]; CANVAS_HEIGHT];

    for cmd in commands {
        match cmd {
            Command::Rect(w, h) => {
                for line in canvas.iter_mut().take(h) {
                    for pix in line.iter_mut().take(w) {
                        *pix = true;
                    }
                }
            }
            Command::RotRow(y, s) => {
                canvas[y].rotate_right(s);
            }
            Command::RotCol(x, s) => {
                let mut prev = [false; CANVAS_HEIGHT];
                for y in 0..canvas.len() {
                    prev[y] = canvas[y][x];
                }

                for (y, pix) in prev.iter().enumerate() {
                    canvas[(y + s) % canvas.len()][x] = *pix;
                }
            }
        }
    }

    canvas
}

fn parse(input: &str) -> impl Iterator<Item = Command> + '_ {
    input.lines().map(|l| {
        if l.starts_with("rect") {
            let mut coords = l.trim_start_matches("rect ").split('x');
            let w = coords.next().unwrap().parse().unwrap();
            let h = coords.next().unwrap().parse().unwrap();

            return Command::Rect(w, h);
        }

        if l.starts_with("rotate column x=") {
            let mut coords = l.trim_start_matches("rotate column x=").split(" by ");
            let x = coords.next().unwrap().parse().unwrap();
            let s = coords.next().unwrap().parse().unwrap();

            return Command::RotCol(x, s);
        }

        if l.starts_with("rotate row y=") {
            let mut coords = l.trim_start_matches("rotate row y=").split(" by ");
            let y = coords.next().unwrap().parse().unwrap();
            let s = coords.next().unwrap().parse().unwrap();

            return Command::RotRow(y, s);
        }

        unreachable!()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day8.txt")), 123);
    }

    #[test]
    fn test_part2() {
        let canvas = part2(include_str!("../input/day8.txt"));
        let expected = r#"
 **  **** ***  *  * ***  **** ***    ** ***   *** 
*  * *    *  * *  * *  *    * *  *    * *  * *    
*  * ***  ***  *  * *  *   *  ***     * *  * *    
**** *    *  * *  * ***   *   *  *    * ***   **  
*  * *    *  * *  * *    *    *  * *  * *       * 
*  * *    ***   **  *    **** ***   **  *    ***  "#
            .trim_start_matches('\n');

        assert_eq!(
            canvas.join("\n"),
            expected,
            "canvas:\n{}",
            canvas.join("\n")
        );
    }
}

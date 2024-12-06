use std::collections::HashSet;
use std::str::FromStr;

struct Particle {
    position: (i64, i64),
    velocity: (i64, i64),
}

pub fn solution(input: &str) -> (Vec<String>, i64) {
    let particles: Vec<Particle> = input.lines().map(|l| l.parse().unwrap()).collect();

    let mut prev_area = bbox_area(particles.iter().map(|p| p.position));
    let mut best_t = 0;

    for t in 1.. {
        let area = bbox_area(particles.iter().map(move |p| p.position_at(t)));

        // hopefully the first minimum area was at the good t
        if area > prev_area {
            break;
        }

        prev_area = area;
        best_t = t;
    }

    let positions = particles
        .iter()
        .map(|p| p.position_at(best_t))
        .collect::<HashSet<_>>();

    let (min_x, min_y, max_x, max_y) = get_bbox(positions.iter().cloned());

    let frame = (min_y..=max_y)
        .map(|y| {
            (min_x..=max_x)
                .map(|x| {
                    if positions.contains(&(x, y)) {
                        '*'
                    } else {
                        ' '
                    }
                })
                .collect::<String>()
        })
        .collect();

    (frame, best_t)
}

fn bbox_area(particles: impl IntoIterator<Item = (i64, i64)>) -> i64 {
    let (min_x, min_y, max_x, max_y) = get_bbox(particles);

    (max_x - min_x) * (max_y - min_y)
}

fn get_bbox(particles: impl IntoIterator<Item = (i64, i64)>) -> (i64, i64, i64, i64) {
    particles.into_iter().fold(
        (
            i64::max_value(),
            i64::max_value(),
            i64::min_value(),
            i64::min_value(),
        ),
        |(mix, miy, max, may), (x, y)| (mix.min(x), miy.min(y), max.max(x), may.max(y)),
    )
}

impl Particle {
    fn position_at(&self, t: i64) -> (i64, i64) {
        (
            self.position.0 + self.velocity.0 * t,
            self.position.1 + self.velocity.1 * t,
        )
    }
}

impl FromStr for Particle {
    type Err = ();

    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let (position, velocity) = inp.split_at(25);
        let position = position.trim().trim_start_matches("position=").trim();
        let velocity = velocity.trim().trim_start_matches("velocity=").trim();

        let parse_pair = |s: &str| -> (i64, i64) {
            let mut parts = s
                .trim_start_matches('<')
                .trim_end_matches('>')
                .trim()
                .split(',');
            let x = parts.next().unwrap().trim().parse().unwrap();
            let y = parts.next().unwrap().trim().parse().unwrap();
            (x, y)
        };

        let position = parse_pair(position);
        let velocity = parse_pair(velocity);
        Ok(Particle { position, velocity })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn solution() {
        let (frame, t) = super::solution(include_str!("../input/day10.txt"));

        assert_eq!(
            frame,
            vec![
                "*    *  ******   ****   *    *  *****   *****   ******  ***** ",
                "*    *  *       *    *  *    *  *    *  *    *       *  *    *",
                " *  *   *       *        *  *   *    *  *    *       *  *    *",
                " *  *   *       *        *  *   *    *  *    *      *   *    *",
                "  **    *****   *         **    *****   *****      *    ***** ",
                "  **    *       *         **    *    *  *         *     *    *",
                " *  *   *       *        *  *   *    *  *        *      *    *",
                " *  *   *       *        *  *   *    *  *       *       *    *",
                "*    *  *       *    *  *    *  *    *  *       *       *    *",
                "*    *  ******   ****   *    *  *****   *       ******  ***** ",
            ]
        );
        assert_eq!(t, 10124);
    }
}

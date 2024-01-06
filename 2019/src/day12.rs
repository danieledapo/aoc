#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Moon {
    position: (i32, i32, i32),
    velocity: (i32, i32, i32),
}

pub fn part1(input: &str, iterations: usize) -> i32 {
    let mut moons = parse(input).collect::<Vec<_>>();

    for _ in 0..iterations {
        for i in 0..moons.len() {
            for j in (i + 1)..moons.len() {
                let gravity = moons[i].gravity(&moons[j]);

                moons[i].apply_gravity(gravity);
                moons[j].apply_gravity((-gravity.0, -gravity.1, -gravity.2));
            }
        }

        for moon in &mut moons {
            moon.update_pos();
        }
    }

    moons.iter().map(Moon::energy).sum()
}

pub fn part2(input: &str) -> usize {
    let mut moons = parse(input).collect::<Vec<_>>();

    let mut x_seen = moons.iter().map(Moon::xs).collect::<Vec<_>>();
    let mut y_seen = moons.iter().map(Moon::ys).collect::<Vec<_>>();
    let mut z_seen = moons.iter().map(Moon::zs).collect::<Vec<_>>();

    let mut x_repeats_at = None;
    let mut y_repeats_at = None;
    let mut z_repeats_at = None;

    for g in 1.. {
        for i in 0..moons.len() {
            for j in (i + 1)..moons.len() {
                let gravity = moons[i].gravity(&moons[j]);

                moons[i].apply_gravity(gravity);
                moons[j].apply_gravity((-gravity.0, -gravity.1, -gravity.2));
            }
        }

        for moon in &mut moons {
            moon.update_pos();
        }

        let check_repeats = |repeats_at: &mut Option<usize>,
                             seen: &mut Vec<(i32, i32)>,
                             prj: Box<dyn Fn(&Moon) -> (i32, i32)>| {
            if repeats_at.is_none() && seen.iter().copied().eq(moons.iter().map(|m| prj(m))) {
                *repeats_at = Some(g);
            }
        };

        check_repeats(&mut x_repeats_at, &mut x_seen, Box::new(Moon::xs));
        check_repeats(&mut y_repeats_at, &mut y_seen, Box::new(Moon::ys));
        check_repeats(&mut z_repeats_at, &mut z_seen, Box::new(Moon::zs));

        if let (Some(x), Some(y), Some(z)) = (x_repeats_at, y_repeats_at, z_repeats_at) {
            let xy = x * y / gcd(x, y);
            return xy * z / gcd(xy, z);
        }
    }

    unreachable!()
}

impl Moon {
    fn gravity(&self, m: &Moon) -> (i32, i32, i32) {
        let dx = -(self.position.0 - m.position.0).signum();
        let dy = -(self.position.1 - m.position.1).signum();
        let dz = -(self.position.2 - m.position.2).signum();

        (dx, dy, dz)
    }

    fn apply_gravity(&mut self, (dx, dy, dz): (i32, i32, i32)) {
        self.velocity.0 += dx;
        self.velocity.1 += dy;
        self.velocity.2 += dz;
    }

    fn update_pos(&mut self) {
        self.position.0 += self.velocity.0;
        self.position.1 += self.velocity.1;
        self.position.2 += self.velocity.2;
    }

    fn energy(&self) -> i32 {
        self.potential_energy() * self.kinetic_energy()
    }

    fn potential_energy(&self) -> i32 {
        self.position.0.abs() + self.position.1.abs() + self.position.2.abs()
    }

    fn kinetic_energy(&self) -> i32 {
        self.velocity.0.abs() + self.velocity.1.abs() + self.velocity.2.abs()
    }

    fn xs(&self) -> (i32, i32) {
        (self.position.0, self.velocity.0)
    }

    fn ys(&self) -> (i32, i32) {
        (self.position.1, self.velocity.1)
    }

    fn zs(&self) -> (i32, i32) {
        (self.position.2, self.velocity.2)
    }
}

fn gcd(mut m: usize, mut n: usize) -> usize {
    while m != 0 {
        let old_m = m;
        m = n % m;
        n = old_m;
    }
    n
}

fn parse(input: &str) -> impl Iterator<Item = Moon> + '_ {
    input.lines().map(|l| {
        let mut cs = l.trim_start_matches('<').trim_end_matches('>').split(", ");

        let x = cs.next().unwrap()[2..].parse().unwrap();
        let y = cs.next().unwrap()[2..].parse().unwrap();
        let z = cs.next().unwrap()[2..].parse().unwrap();

        Moon {
            position: (x, y, z),
            velocity: (0, 0, 0),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(include_str!("../input/day12.txt"), 1000), 7928);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(include_str!("../input/day12.txt")), 518311327635164);
    }
}

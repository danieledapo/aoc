use std::collections::HashMap;

pub fn solution(input: &str) -> (usize, Option<u64>) {
    let mut grid = HashMap::new();
    let mut claims = vec![];

    for l in input.lines() {
        let mut parts = l
            .trim_start_matches('#')
            .split(|c: char| c.is_whitespace() || c == '@' || c == ',' || c == ':' || c == 'x')
            .filter(|s| !s.is_empty());

        let mut next_u64 = || parts.next().unwrap().parse::<u64>().unwrap();

        let id = next_u64();
        let origin_x = next_u64();
        let origin_y = next_u64();
        let width = next_u64();
        let height = next_u64();

        claims.push((id, origin_x, origin_y, width, height));

        for (x, y) in points_in_rec(origin_x, origin_y, width, height) {
            *grid.entry((x, y)).or_insert(0_usize) += 1;
        }
    }

    let overlapped_count = grid.values().filter(|&&c| c > 1).count();

    let non_overlapping = claims
        .into_iter()
        .find(|&(_, ox, oy, w, h)| points_in_rec(ox, oy, w, h).all(|(x, y)| grid[&(x, y)] == 1))
        .map(|(id, _, _, _, _)| id);

    (overlapped_count, non_overlapping)
}

fn points_in_rec(ox: u64, oy: u64, w: u64, h: u64) -> impl Iterator<Item = (u64, u64)> {
    (ox..ox + w).flat_map(move |x| (oy..oy + h).map(move |y| (x, y)))
}

#[cfg(test)]
mod tests {
    #[test]
    fn solution() {
        assert_eq!(
            (116_140, Some(574)),
            super::solution(include_str!("../input/day3.txt"))
        );
    }
}

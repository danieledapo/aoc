//!
//! This solution is based on the [summed-area-table][Summed area table]
//! algorithm.
//!
//! The main idea is to precalculate a sum table where each point stores the
//! area of the rectangle that has the point as the bottom right vertex. This
//! allows to calculate the area of a portion of the space in constant time.
//!
//! As an example, let's consider the following 1D cumulative sum array(it's
//! analogous in 2D or any dimensions really):
//!
//!   input array:     0 1 2 3  4  5
//!   cumulative sum:  0 1 3 6 10 15
//!
//! To retrieve the i-th element of the input array from the cumulative sum
//! array we can just use `sum_array[i] - sum_array[i - 1]`. Expanding this
//! idea, we can get the sum of the range of `s` elements starting from index
//! `i` with `sum_array[i] - sum_array[i - 1 - size]`.
//!
//! In 2D the idea is the same, the only difference is that each point stores
//! the area of a rectangle and we need to pay a bit more attention to not count
//! the same portion of space twice.
//!
//! [summed-area-table]: https://en.wikipedia.org/wiki/Summed-area_table
//!

const TABLE_SIZE: usize = 300;

type SummedAreaTable = [[i32; TABLE_SIZE]; TABLE_SIZE];

pub fn part1(grid_serial_number: i32) -> (usize, usize) {
    let table = summed_area_table(grid_serial_number);

    point_with_largest_area(&table, 3).1
}

pub fn part2(grid_serial_number: i32) -> (usize, (usize, usize)) {
    let table = summed_area_table(grid_serial_number);

    let ((_area, pos), s) = (1..TABLE_SIZE)
        .map(|s| (point_with_largest_area(&table, s), s))
        .max()
        .unwrap();

    (s, pos)
}

fn point_with_largest_area(table: &SummedAreaTable, size: usize) -> (i32, (usize, usize)) {
    let (area, (x, y)) = (0..TABLE_SIZE - size)
        .flat_map(|x| (0..TABLE_SIZE - size).map(move |y| (x, y)))
        .map(|p| (area(p, size, &table), p))
        .max()
        .unwrap();

    // +1 because points are 1-based in the exercise
    (area, (x + 1, y + 1))
}

fn summed_area_table(grid_serial_number: i32) -> SummedAreaTable {
    let mut grid = [[0; 300]; 300];

    for i in 1_i32..=300 {
        for j in 1_i32..=300 {
            let x = i as usize - 1;
            let y = j as usize - 1;

            let above_power = if y > 0 { grid[x][y - 1] } else { 0 };
            let left_power = if x > 0 { grid[x - 1][y] } else { 0 };
            let above_left_power = if x > 0 && y > 0 {
                grid[x - 1][y - 1]
            } else {
                0
            };

            grid[x][y] =
                power(i, j, grid_serial_number) + above_power + left_power - above_left_power;
        }
    }

    grid
}

fn area(top_left: (usize, usize), s: usize, table: &SummedAreaTable) -> i32 {
    assert!(s > 0);

    // since subtractions can overflow if `top_left` is on the top or left bar
    // of the space then we must handle those cases separately.

    match top_left {
        (0, 0) => table[s - 1][s - 1],
        (x, 0) => table[x + s - 1][s - 1] - table[x - 1][s - 1],
        (0, y) => table[s - 1][y + s - 1] - table[s - 1][y - 1],
        (x, y) => {
            table[x - 1][y - 1] + table[x + s - 1][y + s - 1]
                - table[x - 1][y + s - 1]
                - table[x + s - 1][y - 1]
        }
    }
}

fn power(x: i32, y: i32, grid_serial_number: i32) -> i32 {
    let rack_id = x + 10;

    let mut power = rack_id * y;
    power += grid_serial_number;
    power *= rack_id;
    power = (power / 100) % 10 - 5;
    power
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!((33, 45), part1(18));
        assert_eq!((21, 61), part1(42));
        assert_eq!((21, 53), part1(6548));
    }

    #[test]
    fn solution_part2() {
        assert_eq!((16, (90, 269)), part2(18));
        assert_eq!((12, (232, 251)), part2(42));
        assert_eq!((12, (233, 250)), part2(6548));
    }
}

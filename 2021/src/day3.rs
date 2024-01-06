pub fn part1(input: &str) -> usize {
    let nums: Vec<_> = parse(input).collect();
    let ones_freqs = get_ones_freqs(&nums);

    let mut gamma = 0;
    let mut epsilon = 0;

    for (i, f) in ones_freqs.iter().enumerate() {
        let zero_freq = nums.len() - f;
        if *f > zero_freq {
            gamma |= 1 << i;
        } else {
            epsilon |= 1 << i;
        }
    }

    gamma * epsilon
}

pub fn part2(input: &str) -> usize {
    fn get_n(mut nums: Vec<usize>, msb: bool, bit: usize, nbits: usize) -> usize {
        dbg!(&nums);
        assert!(!nums.is_empty());
        if nums.len() == 1 {
            return nums[0];
        }

        let ones_freqs = get_ones_freqs(&nums);
        let one_freq = ones_freqs[bit];
        let zero_freq = nums.len() - one_freq;
        let bit_value = if msb {
            if one_freq >= zero_freq {
                1
            } else {
                0
            }
        } else {
            if one_freq >= zero_freq {
                0
            } else {
                1
            }
        };

        nums.retain(|n| ((n >> bit) & 1) == bit_value);
        get_n(nums, msb, (bit + nbits - 1) % nbits, nbits)
    }

    let nums: Vec<_> = parse(input).collect();

    let nbits = nums.iter().map(|n| bits(*n).count()).max().unwrap();

    let oxygen_generator_rating = get_n(nums.clone(), true, nbits - 1, nbits);
    let co2_scrubber_rating = get_n(nums, false, nbits - 1, nbits);

    dbg!(oxygen_generator_rating, co2_scrubber_rating);

    oxygen_generator_rating * co2_scrubber_rating
}

fn get_ones_freqs(nums: &[usize]) -> Vec<usize> {
    let mut ones_freqs = vec![];

    for n in nums {
        for (i, b) in bits(*n).enumerate() {
            if i >= ones_freqs.len() {
                ones_freqs.push(b);
            } else if b != 0 {
                ones_freqs[i] += 1;
            }
        }
    }

    ones_freqs
}

fn bits(mut n: usize) -> impl Iterator<Item = usize> {
    std::iter::from_fn(move || {
        if n > 0 {
            let b = n & 1;
            n >>= 1;
            return Some(b);
        }

        None
    })
}

fn parse(input: &str) -> impl Iterator<Item = usize> + '_ {
    input.lines().map(|l| usize::from_str_radix(l, 2).unwrap())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(2003336, part1(include_str!("../input/day3.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1877139, part2(include_str!("../input/day3.txt")));
    }
}

pub fn lcm(first: u128, second: u128) -> u128 {
    first * second / gcd(first, second)
}

pub fn gcd(first: u128, second: u128) -> u128 {
    let mut max = first;
    let mut min = second;
    if min > max {
        std::mem::swap(&mut max, &mut min);
    }

    loop {
        let res = max % min;
        if res == 0 {
            return min;
        }

        max = min;
        min = res;
    }
}

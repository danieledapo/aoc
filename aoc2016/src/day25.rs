pub fn part1() -> u32 {
    println!(
        r#"
Solve it by manually inspecting the assembunny code. What the code is actually
doing is printing out the binary representation of the a register plus a factor.
Find the factor in the assembunny (usually the first couple of cpy instructions)
and paste them below.
"#
    );

    let b = 170;
    let c = 15;

    let n: u32 = b * c;
    let bn: u32 = 0xaa_aa_aa_aa >> n.leading_zeros();

    bn - n
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 180);
    }
}

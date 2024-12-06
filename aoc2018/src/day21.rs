use std::collections::HashSet;

///
/// Disassembly
///
/// ```asm
///     #ip 2
///
///  0: seti      123        0 3 ; r3 = 123
///
///  1: bani        3      456 3 ; r3 &= 456
///  2: eqri        3       72 3 ; r3 = r3 == 72
///  3: addr        3        2 2 ; if r3 goto 5 else 4
///  4: seti        0        0 2 ; goto 1
///
///  5: seti        0        0 3 ; r3 = 0
///
///  6: bori        3    65536 4 ; r4 = r3 | 65536
///  7: seti 10649702        3 3 ; r3 = 10649702
///
///  8: bani        4      255 5 ; r5 = r4 & 255
///  9: addr        3        5 3 ; r3 += r5
/// 10: bani        3 16777215 3 ; r3 &= 16777215
/// 11: muli        3    65899 3 ; r3 *= 65899
/// 12: bani        3 16777215 3 ; r3 &= 16777215
/// 13: gtir      256        4 5 ; r5 = 256 > r4
/// 14: addr        5        2 2 ; if r5 then goto 16 else goto 15
///
/// 15: addi        2        1 2 ; goto 17
/// 16: seti       27        7 2 ; goto 28
///
/// 17: seti        0        6 5 ; r5 = 0
///
/// 18: addi        5        1 1 ; r1 = r5 + 1
/// 19: muli        1      256 1 ; r1 *= 256
/// 20: gtrr        1        4 1 ; r1 = r1 > r4
/// 21: addr        1        2 2 ; if r1 then goto 23 else goto 22
///
/// 22: addi        2        1 2 ; goto 24
/// 23: seti       25        9 2 ; goto 26
///
/// 24: addi        5        1 5 ; r5 += 1
/// 25: seti       17        9 2 ; goto 18
///
/// 26: setr        5        7 4 ; r4 = r5
/// 27: seti        7        1 2 ; goto 8
/// 28: eqrr        3        0 5 ; r5 = r3 == r0
/// 28: addr        5        2 2 ; if r5 then halt else goto 29
/// 29: seti        5        4 2 ; goto 6
///
/// ```
///
pub fn program() -> (u64, u64) {
    // let r0: u64 = 0;
    let mut r1: u64;
    // let mut r2 = 0;
    let mut r3: u64;
    let mut r4: u64;
    let mut r5: u64;

    let mut seen = HashSet::new();
    let mut first = None;

    r3 = 123;
    while r3 != 72 {
        r3 &= 456;
    }

    r3 = 0;

    loop {
        let prev_r3 = r3;

        r4 = r3 | 65536;

        r3 = 10_649_702;

        loop {
            r5 = r4 & 255;
            r3 += r5;
            r3 &= 16_777_215;
            r3 *= 65899;
            r3 &= 16_777_215;

            if 256 > r4 {
                break;
            } else {
                r5 = 0;

                loop {
                    r1 = r5 + 1;
                    r1 *= 256;
                    if r1 > r4 {
                        break;
                    } else {
                        r5 += 1;
                    }
                }
            }

            r4 = r5;
        }

        // the first r3 value we see again it's the one that requires the most
        // instructions
        if !seen.insert(r3) {
            return (first.unwrap(), prev_r3);
        }

        if first.is_none() {
            first = Some(r3);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!((10_504_829, 6_311_823), program());
    }
}

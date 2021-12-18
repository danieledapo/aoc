#[derive(Debug)]
pub struct Packet {
    pub version: u8,
    pub packet_id: u8,
    pub size: usize,
    pub data: PacketData,
}

#[derive(Debug)]
pub enum PacketData {
    Literal(u64),
    Operator(Vec<Packet>),
}

pub fn part1(input: &str) -> u64 {
    parse(input).version_sum()
}

pub fn part2(input: &str) -> u64 {
    parse(input).eval()
}

impl Packet {
    fn version_sum(&self) -> u64 {
        let v = u64::from(self.version);
        match &self.data {
            PacketData::Literal(_) => v,
            PacketData::Operator(sub) => v + sub.iter().map(Packet::version_sum).sum::<u64>(),
        }
    }

    fn eval(&self) -> u64 {
        let sub = match &self.data {
            PacketData::Literal(n) => return *n,
            PacketData::Operator(sub) => sub,
        };

        let mut values = sub.iter().map(Packet::eval);

        match self.packet_id {
            0 => values.sum(),
            1 => values.product(),
            2 => values.min().unwrap(),
            3 => values.max().unwrap(),
            5..=7 => {
                let a = values.next().unwrap();
                let b = values.next().unwrap();
                let res;
                if self.packet_id == 5 {
                    res = a > b;
                } else if self.packet_id == 6 {
                    res = a < b;
                } else if self.packet_id == 7 {
                    res = a == b;
                } else {
                    unreachable!()
                }

                if res {
                    1
                } else {
                    0
                }
            }

            _ => unreachable!(),
        }
    }
}

fn parse(input: &str) -> Packet {
    let mut bits = vec![];
    for c in input.trim().chars() {
        let d = c.to_digit(16).unwrap();
        bits.push((d >> 3) & 1);
        bits.push((d >> 2) & 1);
        bits.push((d >> 1) & 1);
        bits.push((d >> 0) & 1);
    }

    let mut bits = bits.into_iter();
    read_packet(&mut bits)
}

fn read_packet(bits: &mut impl Iterator<Item = u32>) -> Packet {
    let version = read_n(bits, 3) as u8;
    let packet_id = read_n(bits, 3) as u8;

    if packet_id == 4 {
        let mut size = 6;
        let mut value = 0;

        loop {
            let c = read_n(bits, 1);
            let d = u64::from(read_n(bits, 4));
            value = (value << 4) | d;
            size += 5;

            if c == 0 {
                break;
            }
        }

        return Packet {
            version,
            packet_id,
            size,
            data: PacketData::Literal(value),
        };
    }

    let length_type_id = read_n(bits, 1);

    if length_type_id == 0 {
        let bytes = read_n(bits, 15) as usize;
        let mut subpackets = vec![];

        let mut size = 0;
        loop {
            let p = read_packet(bits);
            size += p.size;
            subpackets.push(p);
            if size == bytes {
                break;
            }
            assert!(size < bytes);
        }

        return Packet {
            version,
            packet_id,
            size: size + 6 + 1 + 15,
            data: PacketData::Operator(subpackets),
        };
    } else {
        let nsubpackets = read_n(bits, 11);
        let mut size = 6 + 1 + 11;
        let mut subpackets = vec![];

        for _ in 0..nsubpackets {
            let p = read_packet(bits);
            size += p.size;
            subpackets.push(p);
        }

        return Packet {
            version,
            packet_id,
            size,
            data: PacketData::Operator(subpackets),
        };
    }
}

fn read_n(bits: &mut impl Iterator<Item = u32>, n: usize) -> u32 {
    let mut v = 0;
    for b in bits.take(n) {
        v = (v << 1) | b;
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(901, part1(include_str!("../input/day16.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(110434737925, part2(include_str!("../input/day16.txt")));
    }
}

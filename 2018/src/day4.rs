use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct Record<'a> {
    timestamp: &'a str,
    minute: u8,
    kind: RecordKind,
}

#[derive(Debug, PartialEq)]
enum RecordKind {
    ShiftBegin(u64),
    FallsAsleep,
    WakesUp,
}

pub fn solutions(inp: &str) -> (u64, u64) {
    let mut records = inp.lines().map(&Record::new).collect::<Vec<_>>();
    records.sort_by_key(|r| r.timestamp);

    let mut guard_id = None;
    let mut sleep_start = None;
    let mut shifts: HashMap<_, HashMap<u8, u32>> = HashMap::new();

    for r in &records {
        match r.kind {
            RecordKind::ShiftBegin(gid) => guard_id = Some(gid),
            RecordKind::FallsAsleep => sleep_start = Some(r.minute),
            RecordKind::WakesUp => {
                let guard_freqs = shifts.entry(guard_id.unwrap()).or_default();

                for m in sleep_start.unwrap()..r.minute {
                    *guard_freqs.entry(m).or_default() += 1;
                }

                sleep_start = None;
            }
        }
    }

    (part1(&shifts), part2(&shifts))
}

fn part1(shifts: &HashMap<u64, HashMap<u8, u32>>) -> u64 {
    shifts
        .iter()
        .map(|(guard_id, shifts_freqs)| {
            let (most_frequent_minute, _) =
                shifts_freqs.iter().max_by_key(|&(_, freq)| freq).unwrap();

            let total_shifts_time: u32 = shifts_freqs.iter().map(|(_, f)| f).sum();

            (
                total_shifts_time,
                u64::from(*most_frequent_minute) * guard_id,
            )
        })
        .max()
        .unwrap()
        .1
}

fn part2(shifts: &HashMap<u64, HashMap<u8, u32>>) -> u64 {
    shifts
        .iter()
        .map(|(guard_id, shifts_freqs)| {
            let (most_frequent_minute, max_freq) =
                shifts_freqs.iter().max_by_key(|&(_, freq)| freq).unwrap();

            (max_freq, u64::from(*most_frequent_minute) * guard_id)
        })
        .max()
        .unwrap()
        .1
}

impl<'a> Record<'a> {
    fn new(line: &'a str) -> Self {
        let (timestamp, rest) = line.split_at(18);
        let timestamp = timestamp.trim_start_matches('[').trim_end_matches(']');
        let minute = timestamp
            .rsplit(|c| c == ':')
            .next()
            .unwrap()
            .parse::<u8>()
            .unwrap();

        let mut rest_parts = rest.split_whitespace();
        let kind = match rest_parts.next().unwrap() {
            "Guard" => RecordKind::ShiftBegin(
                rest_parts
                    .next()
                    .unwrap()
                    .trim_start_matches('#')
                    .parse()
                    .unwrap(),
            ),
            "falls" => RecordKind::FallsAsleep,
            "wakes" => RecordKind::WakesUp,
            _ => unimplemented!(),
        };

        Record {
            timestamp,
            minute,
            kind,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution() {
        assert_eq!(
            (21956, 134_511),
            solutions(include_str!("../input/day4.txt"))
        );
    }
}

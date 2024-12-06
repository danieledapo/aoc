use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
    ops::RangeInclusive,
};

struct Document<'s> {
    fields: Fields<'s>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

type Fields<'s> = BTreeMap<&'s str, Vec<RangeInclusive<u64>>>;
type Ticket = Vec<u64>;

pub fn part1(input: &str) -> u64 {
    let document = Document::try_from(input).unwrap();

    document
        .nearby_tickets
        .iter()
        .map(|ticket| ticket_scanning_error_rate(ticket, &document.fields))
        .sum()
}

pub fn part2(input: &str) -> u64 {
    let mut document = Document::try_from(input).unwrap();

    {
        // remove invalid tickets
        let fields = &document.fields;
        document
            .nearby_tickets
            .retain(|ticket| ticket_scanning_error_rate(ticket, &fields) == 0);
    }

    let mut initial_solution =
        vec![document.fields.keys().collect::<BTreeSet<_>>(); document.my_ticket.len()];
    for ticket in document
        .nearby_tickets
        .iter()
        .chain(Some(&document.my_ticket))
    {
        for (ps, n) in initial_solution.iter_mut().zip(ticket) {
            for f in ps.clone() {
                if !document.fields[f].iter().any(|r| r.contains(n)) {
                    ps.remove(f);
                }
            }
        }
    }

    let mut solutions = vec![initial_solution];
    let mut seen = BTreeSet::new();

    while let Some(mut solution) = solutions.pop() {
        // if a cell has only a solution, remove it from the other cells
        loop {
            let mut changed = false;
            for i in 0..solution.len() {
                if solution[i].len() != 1 {
                    continue;
                }

                let to_remove = *solution[i].iter().next().unwrap();
                for (j, ps) in solution.iter_mut().enumerate() {
                    if i == j {
                        continue;
                    }
                    changed = ps.remove(to_remove) || changed;
                }
            }
            if !changed {
                break;
            }
        }

        if !seen.insert(solution.clone()) {
            continue;
        }

        if solution.iter().all(|ps| ps.len() == 1) {
            // found a valid solution
            return solution
                .iter()
                .zip(&document.my_ticket)
                .map(|(fs, n)| {
                    let f = fs.iter().next().unwrap();
                    if f.starts_with("departure") {
                        *n
                    } else {
                        1
                    }
                })
                .product();
        }

        // check if solution is valid, albeit not final
        let is_valid = |ticket: &Ticket| -> bool {
            ticket.iter().zip(&solution).all(|(n, ps)| {
                ps.iter()
                    .flat_map(|f| &document.fields[*f])
                    .any(|r| r.contains(n))
            })
        };

        let valid = is_valid(&document.my_ticket) && document.nearby_tickets.iter().all(is_valid);
        if !valid {
            continue;
        }

        // not a final solution, take the field with the smallest number of
        // possibilities and try each one of them
        let i = (0..solution.len())
            .filter(|i| solution[*i].len() > 1)
            .min_by_key(|i| solution[*i].len())
            .unwrap();

        for f in &solution[i] {
            let mut new_solution = solution.clone();
            new_solution[i].clear();
            new_solution[i].insert(f);
            solutions.push(new_solution);
        }
    }

    unreachable!()
}

fn ticket_scanning_error_rate(ticket: &Ticket, fields: &Fields) -> u64 {
    ticket
        .iter()
        .filter(|n| fields.values().flatten().all(|r| !r.contains(n)))
        .sum()
}

impl<'s> TryFrom<&'s str> for Document<'s> {
    type Error = ();

    fn try_from(s: &'s str) -> Result<Self, Self::Error> {
        let mut lines = s.lines().fuse();
        let mut fields = BTreeMap::new();
        while let Some(l) = lines.next() {
            if l.is_empty() {
                break;
            }

            let mut parts = l.split(": ");
            let name = parts.next().ok_or(())?;
            let ranges: Option<Vec<_>> = parts
                .next()
                .ok_or(())?
                .split(" or ")
                .map(|r| {
                    let mut nums = r.split('-');
                    let n0 = nums.next()?.parse().ok()?;
                    let n1 = nums.next()?.parse().ok()?;

                    Some(n0..=n1)
                })
                .collect();

            fields.insert(name, ranges.ok_or(())?);
        }

        if lines.next() != Some("your ticket:") {
            return Err(());
        }
        let my_ticket = lines
            .next()
            .ok_or(())?
            .split(',')
            .map(|c| c.parse().ok())
            .collect::<Option<Ticket>>()
            .ok_or(())?;

        // eat empty line
        if lines.next() != Some("") {
            return Err(());
        }

        if lines.next() != Some("nearby tickets:") {
            return Err(());
        }

        let nearby_tickets = lines
            .map(|l| {
                l.split(',')
                    .map(|c| c.parse().map_err(|_| ()))
                    .collect::<Result<Ticket, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            fields,
            my_ticket,
            nearby_tickets,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(26941, part1(include_str!("../input/day16.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(634796407951, part2(include_str!("../input/day16.txt")));
    }
}

use std::collections::HashMap;

pub fn play(nplayers: usize, turns: usize) -> usize {
    let mut scores = HashMap::with_capacity(nplayers);

    let mut marbles = Marbles::with_capacity(turns);
    let mut cursor = marbles.insert_after(0, 0);

    for i in 1..=turns {
        if i % 23 == 0 {
            let to_remove = (0..7).fold(cursor, |c, _| marbles.prev_of(c));

            let player = i % nplayers;
            *scores.entry(player).or_default() += i + marbles.at(to_remove);

            cursor = marbles.next_of(to_remove);
            marbles.remove(to_remove);
        } else {
            cursor = marbles.insert_after(marbles.next_of(cursor), i);
        }
    }

    scores.values().cloned().max().unwrap()
}

/// Circular linked list backed by a vector. This thing doesn't free removed
/// marbles until the whole struct is dropped for efficiency. It would be
/// possible to use a HashMap that allowing removing elements without shifting,
/// but performance would suffer a bit. Also, probably `indexlist` would be
/// faster.
#[derive(Debug, PartialEq)]
pub struct Marbles {
    nodes: Vec<Marble>,
    root: Option<usize>,
    next_id: usize,
}

#[derive(Debug, PartialEq)]
struct Marble {
    next: usize,
    prev: usize,
    value: usize,
}

impl Marbles {
    pub fn with_capacity(cap: usize) -> Self {
        Marbles {
            nodes: Vec::with_capacity(cap),
            root: None,
            next_id: 0,
        }
    }

    pub fn at(&self, at: usize) -> usize {
        self.nodes[at].value
    }

    pub fn next_of(&self, at: usize) -> usize {
        self.nodes[at].next
    }

    pub fn prev_of(&self, at: usize) -> usize {
        self.nodes[at].prev
    }

    pub fn insert_after(&mut self, at: usize, value: usize) -> usize {
        let new_node_id = self.next_id;
        self.next_id += 1;

        match self.root {
            None => {
                let n = Marble {
                    value,
                    next: 0,
                    prev: 0,
                };
                self.root = Some(new_node_id);
                self.nodes.insert(new_node_id, n);
            }
            Some(_) => {
                let new_node = Marble {
                    value,
                    next: self.nodes[at].next,
                    prev: at,
                };

                self.nodes.insert(new_node_id, new_node);

                let n = self.nodes[at].next;
                self.nodes[n].prev = new_node_id;

                self.nodes[at].next = new_node_id;
            }
        }

        new_node_id
    }

    pub fn remove(&mut self, at: usize) {
        let removed_node_next = self.nodes[at].next;
        let removed_node_prev = self.nodes[at].prev;

        self.nodes[removed_node_prev].next = removed_node_next;
        self.nodes[removed_node_next].prev = removed_node_prev;

        if self.root.unwrap() == at {
            self.root = Some(removed_node_next);
        }
    }

    pub fn debug_view(&self) -> String {
        let mut s = String::new();

        if let Some(root_cursor) = self.root {
            let mut c = root_cursor;

            loop {
                s.push_str(&self.at(c).to_string());
                s.push(' ');

                c = self.next_of(c);
                if c == root_cursor {
                    break;
                }
            }
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(384288, play(455, 71223));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(3189426841, play(455, 7122300));
    }
}

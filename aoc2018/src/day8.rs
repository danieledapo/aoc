struct Node {
    children: Vec<Node>,
    metadata: Vec<u16>,
}

pub fn part1(input: &str) -> u16 {
    fn sum(n: &Node) -> u16 {
        n.metadata.iter().cloned().sum::<u16>() + n.children.iter().map(sum).sum::<u16>()
    }

    let tree = parse_tree(input);
    sum(&tree)
}

pub fn part2(input: &str) -> u16 {
    fn sum(n: &Node) -> u16 {
        if n.children.is_empty() {
            n.metadata.iter().cloned().sum()
        } else {
            n.metadata
                .iter()
                .filter(|&&i| i > 0)
                .flat_map(|i| n.children.get(usize::from(*i - 1)))
                .map(sum)
                .sum()
        }
    }

    let tree = parse_tree(input);
    sum(&tree)
}

fn parse_tree(input: &str) -> Node {
    let mut nums = input.split_whitespace().map(|c| c.parse::<u16>().unwrap());
    parse_node(&mut nums)
}

fn parse_node(nums: &mut impl Iterator<Item = u16>) -> Node {
    let n_children = nums.next().unwrap();
    let n_metadata = nums.next().unwrap();

    let children = (0..n_children).map(|_| parse_node(nums)).collect();
    let metadata = nums.take(usize::from(n_metadata)).collect();

    Node { children, metadata }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solution_part1() {
        assert_eq!(37262, part1(include_str!("../input/day8.txt")));
    }

    #[test]
    fn solution_part2() {
        assert_eq!(66, part2("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"));
        assert_eq!(20839, part2(include_str!("../input/day8.txt")));
    }
}

use std::collections::{BTreeSet, HashMap, HashSet};

type Tile = Vec<Vec<bool>>;

// (tile_id, transformation_id)
type TileOptId = (u64, usize);
type Tiled = Vec<Vec<TileOptId>>;
type TileOpts = HashMap<TileOptId, Tile>;
type BordersMap = HashMap<TileOptId, [Vec<bool>; 4]>;

static MONSTER_PATTERN: &str = r"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ";

pub fn part1(input: &str) -> u64 {
    let tiles = parse(input);
    let (_opts, tiled) = connect_tiles(&tiles);

    tiled[0][0].0
        * tiled[0].last().unwrap().0
        * tiled.last().unwrap()[0].0
        * tiled.last().unwrap().last().unwrap().0
}

pub fn part2(input: &str) -> usize {
    let tiles = parse(input);
    let (opts, tiled) = connect_tiles(&tiles);

    let img = remove_borders(&opts, &tiled);

    get_transforms(&img)
        .iter()
        .filter_map(|opt| {
            let monster_spots = find_monster(&opt);
            if monster_spots.is_empty() {
                None
            } else {
                let mut all_spots = HashSet::new();
                for (y, row) in opt.iter().enumerate() {
                    for (x, c) in row.iter().enumerate() {
                        if *c {
                            all_spots.insert((x, y));
                        }
                    }
                }
                Some(all_spots.difference(&monster_spots).count())
            }
        })
        .next()
        .unwrap()
}

fn find_monster(tile: &Tile) -> HashSet<(usize, usize)> {
    let mut monster_locs = HashSet::new();
    let (mut max_x, mut max_y) = (0, 0);
    for (y, line) in MONSTER_PATTERN.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != '#' {
                continue;
            }

            monster_locs.insert((x, y));
            max_x = x.max(max_x);
            max_y = y.max(max_y);
        }
    }

    let mut monster_spots = HashSet::new();
    let d = tile.len();
    for y in 0..(d - max_y) {
        for x in 0..(d - max_x) {
            let is_monster = monster_locs.iter().all(|&(dx, dy)| tile[y + dy][x + dx]);

            if is_monster {
                monster_spots.extend(monster_locs.iter().map(|&(dx, dy)| (x + dx, y + dy)));
            }
        }
    }

    monster_spots
}

fn connect_tiles(tiles: &HashMap<u64, Tile>) -> (TileOpts, Tiled) {
    let opts: HashMap<_, _> = tiles
        .iter()
        .flat_map(|(&k, t)| {
            get_transforms(t)
                .into_iter()
                .enumerate()
                .map(move |(xid, t)| ((k, xid), t))
        })
        .collect();

    let borders: BordersMap = opts
        .iter()
        .map(|(tid, tile)| (*tid, get_borders(tile)))
        .collect();

    let d = (tiles.len() as f64).sqrt() as usize;
    assert_eq!(d * d, tiles.len());

    let mut tiled = vec![vec![(u64::max_value(), usize::max_value()); d]; d];
    let ok = do_connect(&mut tiled, &borders, d, 0, 0, &mut HashSet::new());

    assert!(ok);

    (opts, tiled)
}

fn do_connect(
    tiled: &mut Tiled,
    borders: &BordersMap,
    dimension: usize,
    x: usize,
    y: usize,
    seen: &mut HashSet<u64>,
) -> bool {
    if y == dimension {
        return true;
    }

    let (mut nextx, mut nexty) = (x + 1, y);
    if nextx == dimension {
        nextx = 0;
        nexty += 1;
    }

    for (&(id, trans_id), border) in borders {
        if seen.contains(&id) {
            continue;
        }

        let [top, _, _, left] = border;
        if x > 0 {
            let neighbor = tiled[y][x - 1];
            let [_, neighbor_right, _, _] = &borders[&neighbor];
            if neighbor_right != left {
                continue;
            }
        }

        if y > 0 {
            let neighbor = tiled[y - 1][x];
            let [_, _, neighbor_bottom, _] = &borders[&neighbor];
            if neighbor_bottom != top {
                continue;
            }
        }

        seen.insert(id);
        tiled[y][x] = (id, trans_id);
        let ok = do_connect(tiled, borders, dimension, nextx, nexty, seen);
        if ok {
            return ok;
        }

        seen.remove(&id);
    }

    false
}

fn remove_borders(tiles: &TileOpts, tiled: &Tiled) -> Tile {
    let tile_size = tiles.values().next().unwrap().len();

    let mut out: Tile = vec![];

    for row in tiled {
        for y in 1..tile_size - 1 {
            let v = row
                .iter()
                .flat_map(|tid| &tiles[tid][y][1..tile_size - 1])
                .copied()
                .collect();

            out.push(v);
        }
    }

    out
}

fn get_borders(tile: &Tile) -> [Vec<bool>; 4] {
    [
        tile[0].clone(),
        tile.iter().map(|r| *r.last().unwrap()).collect(),
        tile.last().unwrap().clone(),
        tile.iter().map(|r| r[0]).collect(),
    ]
}

fn get_transforms(tile: &Tile) -> BTreeSet<Tile> {
    get_flips(tile).iter().flat_map(|t| get_rots(t)).collect()
}

fn get_rots(tile: &Tile) -> Vec<Tile> {
    let mut out = vec![tile.clone()];
    let mut last = tile.clone();

    for _ in 0..3 {
        let mut t = last.clone();
        for y in 0..t.len() {
            for x in 0..t[y].len() {
                t[y][x] = last[x][t[y].len() - y - 1];
            }
        }
        last = t.clone();
        out.push(t.clone());
    }

    out
}

fn get_flips(tile: &Tile) -> Vec<Tile> {
    let mut out = vec![tile.clone()];

    out.push(tile.iter().rev().cloned().collect());
    out.push(
        tile.iter()
            .map(|r| r.iter().rev().copied().collect())
            .collect(),
    );
    out.push(
        tile.iter()
            .rev()
            .map(|r| r.iter().rev().copied().collect())
            .collect(),
    );

    out
}

fn parse(input: &str) -> HashMap<u64, Tile> {
    let mut lines = input.lines();
    let mut out = HashMap::new();
    while let Some(tilen) = lines.next() {
        let n = tilen
            .trim_start_matches("Tile ")
            .trim_end_matches(':')
            .parse()
            .unwrap();

        let lines = lines
            .by_ref()
            .take_while(|l| !l.is_empty())
            .map(|l| l.chars().map(|c| c == '#').collect())
            .collect();

        out.insert(n, lines);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(47213728755493, part1(include_str!("../input/day20.txt")));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1599, part2(include_str!("../input/day20.txt")));
    }
}

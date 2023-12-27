use nom::error::ErrorKind;
use nom::bytes::complete::tag;
use nom::character::complete::i32 as nomI32;
use nom::sequence::separated_pair;
use nom::multi::separated_list1;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::cmp::max;
use itertools::Itertools;
use num::range;

use crate::day::{Day, DayNum};
use crate::utils::parse;

type Point = (i32, i32, i32);
type BlockSet = HashMap<usize, HashSet<usize>>;

fn parse_line(line: &str) -> (Point, Point) {
    let point_parser1 = separated_list1(tag(","), nomI32);
    let point_parser2 = separated_list1(tag(","), nomI32);
    let points = separated_pair::<_, _, _, _, (_, ErrorKind), _, _, _>
        (point_parser1, tag("~"), point_parser2);

    let (p1, p2) = parse(points, line);

    ((p1[0], p1[1], p1[2]), (p2[0], p2[1], p2[2]))
}

fn drop_blocks(blocks: &mut Vec<(Point, Point)>) -> BlockSet {
    blocks.sort_by_key(|&(_, (_, _, z))| z);
    let mut map: HashMap<(i32, i32), BTreeSet<(usize, i32)>> = HashMap::new();
    let mut supports: HashMap<usize, HashSet<usize>> = HashMap::new();

    for (block, &((x1, y1, z1), (x2, y2, z2))) in blocks.iter().enumerate() {
        let mut max_z = 0;
        for x in x1..x2 + 1 {
            for y in y1..y2 + 1 {
                let &(_, z) = map.entry((x, y)).or_default().last().or(Some(&(0, 0))).unwrap();
                max_z = max(z, max_z);
            }
        }

        for x in x1..x2 + 1 {
            for y in y1..y2 + 1 {
                let support = map.entry((x, y)).or_default().last();
                if let Some(&(block_id, z)) = support {
                    if z == max_z {
                        supports.entry(block_id).or_default().insert(block);
                    }
                }
                for z in 0..z2 + 1 - z1 {
                    map.entry((x, y)).or_default().insert((block, z + max_z + 1));
                }
            }
        }
    }

    supports
}

fn is_supported_by(blocks: usize, supports: &BlockSet) -> BlockSet {
    range(0, blocks)
        .map(|b| (b, supports.clone()
            .into_iter()
            .filter_map(|(k, v)| if v.contains(&b) { Some(k) } else { None })
            .unique()
            .collect()))
        .collect()
}

pub struct Day22 {}

impl Day for Day22 {
    type Input = Vec<(Point, Point)>;
    type Result = usize;

    fn day(&self) -> DayNum { 22 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let mut blocks = input.clone();

        let supports = drop_blocks(&mut blocks);
        blocks.len() - is_supported_by(blocks.len(), &supports).values()
            .filter(|&v| v.len() == 1)
            .map(|s| s.iter().collect_vec())
            .concat()
            .iter()
            .unique()
            .count()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let mut blocks = input.clone();

        let supports = drop_blocks(&mut blocks);
        let supported_by = is_supported_by((&blocks).len(), &supports);

        range(0, blocks.len()).map(|block| {
            let mut removed_blocks = HashSet::from([block]);

            while {
                let new_removed: HashSet<usize> = removed_blocks.iter()
                    .filter_map(|removed| supports.get(removed))
                    .map(|s| s.into_iter().collect_vec())
                    .concat()
                    .into_iter()
                    .unique()
                    .filter(|&removed_by|
                        supported_by[removed_by].iter().all(|x| removed_blocks.contains(x)))
                    .map(|&x| x)
                    .collect::<HashSet<_>>()
                    .union(&removed_blocks)
                    .map(|&x| x)
                    .collect();

                let diff = new_removed != removed_blocks;
                removed_blocks = new_removed;
                diff
            } {}

            removed_blocks.len() - 1
        }).sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day22::Day22;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "1,0,1~1,2,1
         0,0,2~2,0,2
         0,2,3~2,2,3
         0,0,4~0,2,4
         2,0,5~2,2,5
         0,1,6~2,1,6
         1,1,8~1,1,9"};

    #[test]
    fn test22_1() {
        let day = Day22 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 5);
    }

    #[test]
    fn test22_2() {
        let day = Day22 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 7);
    }
}
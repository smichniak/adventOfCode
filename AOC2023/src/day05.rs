use std::cmp::{max, min};
use itertools::Itertools;
use nom::bytes::complete::{tag, take_until, take};
use nom::character::complete::u64 as nomU64;
use nom::sequence::{tuple, preceded, terminated};
use nom::multi::{many0, separated_list0};
use nom::error::ErrorKind;

use crate::day::{Day, DayNum};
use crate::utils::parse;


type Triplet = (u64, u64, u64);
type Block = Vec<Triplet>;
type Blocks = Vec<Block>;

fn parse_seeds(input_str: &str) -> (&str, Vec<u64>) {
    let nums = separated_list0::<_, _, _, (_, ErrorKind), _, _>(tag(" "), nomU64);
    preceded(tag("seeds: "), terminated(nums, tag("\n")))(input_str).unwrap()
}

fn parse_blocks(s: &str) -> Blocks {
    let tuple_parser = tuple::<_, (u64, _, u64, _, u64), (_, ErrorKind), _>;
    let triplet_parser = tuple_parser((nomU64, tag(" "), nomU64, tag(" "), nomU64));

    let triplets = separated_list0(tag("\n"), triplet_parser);
    let block = preceded(take_until(":\n"), preceded(take(2usize), triplets));
    let blocks = many0(block);
    parse(blocks, s).iter()
        .map(|v| v.iter().map(|(n1, _, n2, _, n3)|
            (*n1, *n2, *n3)).collect_vec())
        .collect_vec()
}

fn get_seed_location(seed: u64, blocks: &Blocks) -> u64 {
    let mut curr_val = seed;

    for block in blocks {
        let matching = block.iter()
            .filter(|(_, src, range)| *src <= curr_val && curr_val < *src + *range)
            .collect_vec();
        match matching.get(0) {
            Some((dst, src, _)) => curr_val = dst + curr_val - src,
            None => {}
        }
    }

    curr_val
}

pub struct Day05 {}

impl Day for Day05 {
    type Input = (Vec<u64>, Blocks);
    type Result = u64;

    fn day(&self) -> DayNum { 05 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        let (rest, seeds) = parse_seeds(file_input.as_str());
        (seeds, parse_blocks(rest))
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let (seeds, blocks) = input;
        let locs = seeds.iter().map(|s| get_seed_location(*s, &blocks)).collect_vec();
        locs.into_iter().min().unwrap()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let step: u64 = 10000;
        let (seeds, blocks) = input;

        let mut min_loc = self.solve1((seeds.iter().step_by(2).map(|x| *x).collect(), blocks.clone()));
        let mut good_seeds = Vec::new();


        for (i, seed0) in seeds.iter().enumerate().step_by(2) {
            let step = if seeds[i + 1] > 1000000 {step} else {1};
            for seed in (*seed0..*seed0 + seeds[i + 1]).step_by(step as usize) {
                let new_loc = get_seed_location(seed, &blocks);
                if new_loc < min_loc {
                    good_seeds.push((seed, *seed0, *seed0 + seeds[i + 1]));
                    min_loc = new_loc;
                }
            }
        }

        for (seed, seed_min, seed_max) in good_seeds {
            for s in max(seed_min, seed.saturating_sub(step))..min(seed_max, seed + step) {
                min_loc = min(min_loc, get_seed_location(s, &blocks));
            }
        }

        min_loc
    }
}

#[cfg(test)]
mod tests {
    use crate::day05::Day05;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "seeds: 79 14 55 13

         seed-to-soil map:
         50 98 2
         52 50 48

         soil-to-fertilizer map:
         0 15 37
         37 52 2
         39 0 15

         fertilizer-to-water map:
         49 53 8
         0 11 42
         42 0 7
         57 7 4

         water-to-light map:
         88 18 7
         18 25 70

         light-to-temperature map:
         45 77 23
         81 45 19
         68 64 13

         temperature-to-humidity map:
         0 69 1
         1 0 69

         humidity-to-location map:
         60 56 37
         56 93 4"};

    #[test]
    fn test05_1() {
        let day = Day05 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 35);
    }

    #[test]
    fn test05_2() {
        let day = Day05 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 46);
    }
}
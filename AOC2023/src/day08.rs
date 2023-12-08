use nom::bytes::complete::tag;
use nom::character::complete::alphanumeric1;
use nom::sequence::{separated_pair, delimited};
use nom::error::ErrorKind;

use std::collections::HashMap;
use itertools::Itertools;
use num::integer::lcm;

use crate::day::{Day, DayNum};
use crate::utils::parse;

type StrMap = HashMap<String, (String, String)>;

fn parse_line(line: &str) -> (String, (String, String)) {
    let dst = separated_pair(alphanumeric1::<_, (_, ErrorKind)>, tag(", "), alphanumeric1);
    let dst_pair = delimited(tag("("), dst, tag(")"));
    let node = separated_pair(alphanumeric1, tag(" = "), dst_pair);
    let (s1, (s2, s3)) = parse(node, line);
    (s1.to_string(), (s2.to_string(), s3.to_string()))
}

fn get_steps(src: &String, dst: &String, map: &StrMap, steps: &[u8], max_step: Option<usize>) -> Option<u64> {
    let mut curr = src;
    let mut step_num = 0;

    while curr != dst {
        match steps[step_num % steps.len()] as char {
            'L' => curr = &map.get(curr).unwrap().0,
            _ => curr = &map.get(curr).unwrap().1,
        }
        step_num += 1;
        if max_step.is_some_and(|m| step_num > m) {
            return None;
        }
    }

    Some(step_num as u64)
}

pub struct Day08 {}

impl Day for Day08 {
    type Input = (String, StrMap);
    type Result = u64;

    fn day(&self) -> DayNum { 08 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        let lines = file_input.split('\n');
        let moves = lines.clone().take(1).collect_vec()[0].to_string();
        let str_map = lines.skip(2).map(parse_line).collect();

        (moves, str_map)
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let (steps, map) = input;
        get_steps(&"AAA".to_string(), &"ZZZ".to_string(), &map, steps.as_bytes(), None).unwrap()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let (steps, map) = input;
        let step_bytes = steps.as_bytes();
        let max_steps = Some(lcm(steps.len(), map.len()));
        let a_start = map.keys().filter(|s| s.as_bytes()[s.len() - 1] as char == 'A');
        let z_end = map.keys().filter(|s| s.as_bytes()[s.len() - 1] as char == 'Z');

        a_start
            .map(|src| z_end.clone()
                .filter_map(|dst|
                    get_steps(src, dst, &map, &step_bytes, max_steps)).next().unwrap())
            .fold(1, lcm)
    }
}

#[cfg(test)]
mod tests {
    use crate::day08::Day08;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT1: &str = indoc! {
        "LLR

         AAA = (BBB, BBB)
         BBB = (AAA, ZZZ)
         ZZZ = (ZZZ, ZZZ)"};

    const INPUT2: &str = indoc! {
        "LR

         11A = (11B, XXX)
         11B = (XXX, 11Z)
         11Z = (11B, XXX)
         22A = (22B, XXX)
         22B = (22C, 22C)
         22C = (22Z, 22Z)
         22Z = (22B, 22B)
         XXX = (XXX, XXX)"};

    #[test]
    fn test08_1() {
        let day = Day08 {};
        assert_eq!(day.solve1(day.parse_input(INPUT1.to_string())), 6);
    }

    #[test]
    fn test08_2() {
        let day = Day08 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT2.to_string())), 6);
    }
}
use itertools::Itertools;
use nom::multi::separated_list0;
use nom::bytes::complete::tag;
use nom::character::complete::{i32 as nomi32};
use nom::error::ErrorKind;

use crate::day::{Day, DayNum};
use crate::utils::parse;

fn parse_line(line: &str) -> Vec<i32> {
    parse(separated_list0(tag(" "), nomi32::<_, (_, ErrorKind)>), line)
}

fn get_differences(values: Vec<i32>) -> Vec<Vec<i32>> {
    let mut vecs = vec![values];

    while !vecs.last().unwrap().iter().all_equal() {
        vecs.push(vecs.last()
            .unwrap()
            .windows(2)
            .map(|w| w[1] - w[0])
            .collect_vec()
        );
    }

    vecs
}

fn future_prediction(values: Vec<i32>) -> i32 {
    get_differences(values).iter()
        .map(|v| v.last().unwrap())
        .sum()
}

fn past_prediction(values: Vec<i32>) -> i32 {
    get_differences(values).iter()
        .map(|v| v[0])
        .rev()
        .fold(0, |acc, x| x - acc)
}


pub struct Day09 {}

impl Day for Day09 {
    type Input = Vec<Vec<i32>>;
    type Result = i32;

    fn day(&self) -> DayNum { 09 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.into_iter()
            .map(future_prediction)
            .sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        input.into_iter()
            .map(past_prediction)
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day09::Day09;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "0 3 6 9 12 15
         1 3 6 10 15 21
         10 13 16 21 30 45"};

    #[test]
    fn test09_1() {
        let day = Day09 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 114);
    }

    #[test]
    fn test09_2() {
        let day = Day09 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 2);
    }
}
use std::cmp::max;
use std::vec::Vec;

use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha0, u32 as nomU32};
use nom::sequence::{tuple, separated_pair, delimited};
use nom::branch::alt;
use nom::multi::separated_list0;

use crate::day::{Day, DayNum};

pub struct Day02 {}

#[derive(Debug)]
pub enum Color {
    Red,
    Green,
    Blue,
}

impl From<&str> for Color {
    fn from(s: &str) -> Self {
        match s {
            "red" => Color::Red,
            "green" => Color::Green,
            _ => Color::Blue,
        }
    }
}

impl From<&Color> for u32 {
    fn from(col: &Color) -> Self {
        match col {
            Color::Red => 12,
            Color::Green => 13,
            Color::Blue => 14,
        }
    }
}

type InputElement = (u32, Vec<(u32, Color)>);

fn color_parser(input: &str) -> IResult<&str, Color> {
    let (remaining, col) = alpha0(input)?;
    Ok((remaining, col.into()))
}

fn parse_round(input: &str) -> IResult<&str, (u32, Color)> {
    separated_pair(nomU32, tag(" "), color_parser)(input)
}

fn parse_rounds(input: &str) -> IResult<&str, Vec<(u32, Color)>> {
    let delim = alt((tag(", "), tag("; ")));
    separated_list0(delim, parse_round)(input)
}


fn parse_line(line: &str) -> InputElement {
    let start = delimited(tag("Game "), nomU32, tag(": "));
    let (_, (game, rounds)) = tuple((start, parse_rounds))(line).unwrap();
    (game, rounds)
}

impl Day for Day02 {
    type Input = Vec<InputElement>;
    type Result = u32;

    fn day(&self) -> DayNum { 02 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.iter()
            .filter(|(_, v)| {
                v.iter().all(|(num, col)| num <= &u32::from(col))
            })
            .map(|(g, _)| g)
            .sum()
    }
    fn solve2(&self, input: Self::Input) -> Self::Result {
        input.iter()
            .map(|(_, v)| v.iter().fold((0, 0, 0), |(r, g, b), (num, col)| {
                match col {
                    Color::Red => (max(r, *num), g, b),
                    Color::Green => (r, max(g, *num), b),
                    Color::Blue => (r, g, max(b, *num))
                }
            }))
            .map(|(x, y, z)| x * y * z)
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day02::Day02;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"};

    #[test]
    fn test02_1() {
        let day = Day02 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 8);
    }

    #[test]
    fn test02_2() {
        let day = Day02 {};
        assert_eq!(day.solve2(day.parse_input(INPUT.to_string())), 2286);
    }
}
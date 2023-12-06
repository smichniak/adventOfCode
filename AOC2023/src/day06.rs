use itertools::Itertools;
use nom::bytes::complete::take_till;
use nom::character::complete::{space1, u64 as nomU64};
use nom::sequence::preceded;
use nom::multi::separated_list0;
use nom::error::ErrorKind;
use nom::AsChar;

use crate::day::{Day, DayNum};
use crate::utils::parse;

fn parse_line(line: &str) -> Vec<u64> {
    let start = take_till(AsChar::is_dec_digit);
    let nums = separated_list0(space1::<_, (_, ErrorKind)>, nomU64);
    parse(preceded(start, nums), line)
}

fn quadratic_solutions(t: u64, d: u64) -> u64 {
    let tf = t as f64;
    let df = d as f64;

    let delta = (tf * tf - 4.0 * df).sqrt();
    let minus = ((tf - delta) / 2.0 + 1.0).floor() as u64;
    let plus = ((tf + delta) / 2.0 - 1.0).ceil() as u64;
    plus - minus + 1
}

pub struct Day06 {}

impl Day for Day06 {
    type Input = (Vec<u64>, Vec<u64>);
    type Result = u64;

    fn day(&self) -> DayNum { 06 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        let lines = file_input.split('\n').collect_vec();
        (parse_line(lines[0]), parse_line(lines[1]))
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        // Hold for t_0, go for t - t_0
        // v = t_0, s = v * t = t_0 (t - t_0)
        // t_0 (t - t_0) > d
        // t_0^2 - t_0 t + d < 0
        // t_+- = 1/2 (t +- sqrt(t^2 - 4d) )
        // res = ceil(t_+ - 1) - floor(t_- + 1) + 1

        std::iter::zip(input.0.iter(), input.1.iter())
            .map(|(t, d)| quadratic_solutions(*t, *d))
            .product()
    }

    fn parse_input2(&self, file_input: String) -> Self::Input {
        self.parse_input(file_input.replace(" ", ""))
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        self.solve1(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::day06::Day06;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "Time:      7  15   30
         Distance:  9  40  200"};

    #[test]
    fn test06_1() {
        let day = Day06 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 288);
    }

    #[test]
    fn test06_2() {
        let day = Day06 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 71503);
    }
}
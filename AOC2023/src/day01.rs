use onig::Regex;

use crate::day::{Day, DayNum};

const DIGIT_WORDS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

fn parse_line(line: &str) -> Vec<u32> {
    line.chars().filter(|c| c.is_numeric()).map(|c| c.to_digit(10).unwrap()).collect()
}

fn parse_line2(line: &str) -> Vec<u32> {
    let words = DIGIT_WORDS.map(|w| format!("({})", w)).join("|");
    let regex_pattern = format!(r"(?=(\d)|{})", words);
    let regex = Regex::new(regex_pattern.as_str()).unwrap();

    let mut nums = Vec::new();

    regex.captures_iter(line).for_each(|captures|
        captures.iter_pos()
            .skip(1)
            .enumerate()
            .filter(|(_, c)| c.is_some())
            .map(|(group, cap)|
                match group {
                    0 => line.chars().nth(cap.unwrap().0).unwrap().to_digit(10).unwrap(),
                    x => x.try_into().unwrap()
                })
            .for_each(|x| nums.push(x))
    );

    nums
}

pub struct Day01 {}

impl Day for Day01 {
    type Input = Vec<Vec<u32>>;
    type Result = u32;

    fn day(&self) -> DayNum { 01 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.iter().map(|v| 10 * v[0] + v[v.len() - 1]).sum()
    }

    fn parse_input2(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line2).collect()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        self.solve1(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::day01::Day01;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT1: &str = indoc! {
        "1abc2
         pqr3stu8vwx
         a1b2c3d4e5f
         treb7uchet"};

    const INPUT2: &str = indoc! {
        "two1nine
         eightwothree
         abcone2threexyz
         xtwone3four
         4nineeightseven2
         zoneight234
         7pqrstsixteen"};

    #[test]
    fn test01_1() {
        let day = Day01 {};
        assert_eq!(day.solve1(day.parse_input(INPUT1.to_string())), 142);
    }

    #[test]
    fn test01_2() {
        let day = Day01 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT2.to_string())), 281);
    }
}
use std::collections::HashMap;
use regex::Regex;
use itertools::Itertools;

use crate::day::{Day, DayNum};

fn adjacent_matrix(lines: &Vec<&str>) -> Vec<Vec<(usize, usize, char)>> {
    let m = lines.len() as isize;
    let n = lines[0].len() as isize;
    let mut adjacent = vec![vec![(0, 0, '.'); n as usize]; m as usize];
    for (iu, l) in lines.iter().enumerate() {
        for (ju, c) in l.chars().enumerate() {
            let i = iu as isize;
            let j = ju as isize;
            if c != '.' && !c.is_numeric() {
                for di in -1..2isize {
                    for dj in -1..2isize {
                        if 0 <= i + di && i + di < m && 0 <= j + dj && j + dj < n {
                            adjacent[(i + di) as usize][(j + dj) as usize] = (iu, ju, c);
                        }
                    }
                }
            }
        }
    }
    adjacent
}

pub struct Day03 {}

impl Day for Day03 {
    type Input = Vec<(u32, Vec<(usize, usize, char)>)>;
    type Result = u32;

    fn day(&self) -> DayNum { 03 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        let lines = file_input.split('\n').collect_vec();
        let adjacent = adjacent_matrix(&lines);
        let mut result = Vec::new();

        let re = Regex::new(r"\d+").unwrap();
        for (row, line) in lines.iter().enumerate() {
            for captures in re.captures_iter(line) {
                let num_match = captures.get(0).unwrap();
                let adjacent_symbols = adjacent[row][num_match.range()].iter()
                    .copied()
                    .filter(|(_, _, c)| *c != '.' && !c.is_numeric())
                    .dedup()
                    .collect_vec();

                result.push((num_match.as_str().parse().unwrap(), adjacent_symbols));
            }
        }
        result
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.iter().filter(|(_, v)| !v.is_empty())
            .map(|(num, _)| num)
            .sum()
    }
    fn solve2(&self, input: Self::Input) -> Self::Result {
        let mut gears = HashMap::new();

        input.iter()
            .map(|(num, v)| (num, v.iter().filter(|(_, _, c)| *c == '*')))
            .map(|(num, v)| v.map(move |(i, j, _)| (i, j, num)))
            .flatten()
            .for_each(|(i, j, num)|
                gears.entry((*i, *j)).or_insert_with(Vec::new).push(*num)
            );

        gears.iter()
            .filter(|(_, v)| v.len() >= 2)
            .map(|(_, v)| v[0] * v[1])
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day03::Day03;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
    "467..114..
     ...*......
     ..35..633.
     ......#...
     617*......
     .....+.58.
     ..592.....
     ......755.
     ...$.*....
     .664.598.."};

    #[test]
    fn test03_1() {
        let day = Day03 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 4361);
    }

    #[test]
    fn test03_2() {
        let day = Day03 {};
        assert_eq!(day.solve2(day.parse_input(INPUT.to_string())), 467835);
    }
}
use std::isize;
use itertools::Itertools;
use num::abs;

use crate::utils::{Direction, move_straight_distance};
use crate::utils::Direction::{Down, Left, Right, Up};
use crate::day::{Day, DayNum};


impl Into<Direction> for char {
    fn into(self) -> Direction {
        match self {
            'U' => Up,
            'D' => Down,
            'L' => Left,
            'R' => Right,
            _ => unimplemented!()
        }
    }
}

fn parse_line(line: &str) -> (Direction, isize, Direction, isize) {
    let mut line_split = line.split(' ');
    let dir_char = line_split.nth(0).unwrap().chars().nth(0).unwrap();
    let num = line_split.nth(0).unwrap().parse().unwrap();

    let mut color_str = (&line_split.nth(0).unwrap()[2..]).chars();
    let new_dir = match color_str.nth_back(1).unwrap() {
        '0' => Right,
        '1' => Down,
        '2' => Left,
        '3' => Up,
        _ => unimplemented!(),
    };

    (dir_char.into(), num, new_dir, isize::from_str_radix(color_str.as_str(), 16).unwrap())
}

fn get_area(instructions: Vec<(Direction, isize)>) -> isize {
    let (mut prev_i, mut prev_j) = (0, 0);
    let mut area = 0;

    for (dir, len) in instructions {
        let (curr_i, curr_j, _) = move_straight_distance(prev_i, prev_j, dir, len);
        area += (prev_i + curr_i) * (prev_j - curr_j) + len;
        (prev_i, prev_j) = (curr_i, curr_j);
    }

    abs(area) / 2 + 1
}

pub struct Day18 {}

impl Day for Day18 {
    type Input = Vec<(Direction, isize)>;
    type Result = isize;

    fn day(&self) -> DayNum { 18 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n')
            .map(parse_line)
            .map(|(d, l, _, _)| (d, l))
            .collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        get_area(input)
    }

    fn parse_input2(&self, file_input: String) -> Self::Input {
        file_input.split('\n')
            .map(parse_line)
            .map(|(_, _, d, l)| (d, l))
            .collect_vec()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        get_area(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::day18::Day18;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "R 6 (#70c710)
         D 5 (#0dc571)
         L 2 (#5713f0)
         D 2 (#d2c081)
         R 2 (#59c680)
         D 2 (#411b91)
         L 5 (#8ceee2)
         U 2 (#caa173)
         L 1 (#1b58a2)
         U 2 (#caa171)
         R 2 (#7807d2)
         U 3 (#a77fa3)
         L 2 (#015232)
         U 2 (#7a21e3)"};

    #[test]
    fn test18_1() {
        let day = Day18 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 62);
    }

    #[test]
    fn test18_2() {
        let day = Day18 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 952408144115);
    }
}
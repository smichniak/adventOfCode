use std::collections::HashSet;
use itertools::Itertools;

use crate::utils::{DX, DY};
use crate::day::{Day, DayNum};

type Map = Vec<Vec<char>>;
type Point = (i32, i32);

fn get_start(map: &Map) -> Point {
    map.iter()
        .enumerate()
        .filter_map(|(i, l)| l.iter()
            .map(|x| (i, x))
            .find_position(|(_, &x)| x == 'S'))
        .map(|(i, (j, _))| (i as i32, j as i32))
        .nth(0)
        .unwrap()
}

fn valid_point(i: i32, j: i32, map: &Map) -> bool {
    let m = map.len() as i32;
    let n = map[0].len() as i32;

    0 <= i && i < m && 0 <= j && j < n && map[i as usize][j as usize] != '#'
}

fn reachable(map: &Map, initial_points: HashSet<Point>, steps: u32) -> HashSet<Point> {
    let mut points = initial_points.clone();

    for _ in 0..steps {
        let mut new_points = HashSet::new();
        for (i, j) in points {
            for (di, dj) in DX.into_iter().zip(DY) {
                if valid_point(i + di, j + dj, map) {
                    new_points.insert((i + di, j + dj));
                }
            }
        }

        points = new_points;
    }

    return points;
}

fn reachable_num(map: &Map, steps: u32) -> usize {
    let start = get_start(map);
    reachable(map, [start].into(), steps).iter().count()
}

fn reachable_infinite(map: &Map, initial_points: HashSet<Point>, steps: usize) -> HashSet<Point> {
    let m = map.len() as i32;
    let n = map[0].len() as i32;
    let dx = [-1, 1, 0, 0];
    let dy = [0, 0, -1, 1];

    let mut points = initial_points.clone();
    let mut ps = Vec::new();

    for _ in 0..steps {
        ps.push(points.len());
        let mut new_points = HashSet::new();
        for (i, j) in points {
            for (di, dj) in dx.into_iter().zip(dy) {
                if map[(i + di).rem_euclid(m) as usize][(j + dj).rem_euclid(n) as usize] != '#' {
                    new_points.insert((i + di, j + dj));
                }
            }
        }

        points = new_points;
    }

    points
}

fn reachable_infinite_num(map: &Map, steps: usize) -> usize {
    let start = get_start(map);
    reachable_infinite(map, [start].into(), steps).iter().count()
}

pub struct Day21 {}

impl Day for Day21 {
    type Input = Vec<Vec<char>>;
    type Result = usize;

    fn day(&self) -> DayNum { 21 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars().collect()).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        reachable_num(&input, 64)
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        // 26501365 = 65 + 131 * 202300

        // y = a x^2 + b x + c

        // y0 = c
        // y1 = a + b + c
        // y2 = 4 a + 2 b + c

        // y2 - 2 y1 = 2a - c

        // a = (y2 - 2 y1 + y0) / 2
        // b = (4y1 - y2 - 3 y0) / 2
        // c = y0

        let n0 = (input.len() - 1) / 2;

        let y0 = reachable_infinite_num(&input, n0);
        let y1 = reachable_infinite_num(&input, n0 + input.len());
        let y2 = reachable_infinite_num(&input, n0 + 2 * input.len());

        let a2 = y2 + y0 - 2 * y1;
        let b2 = 4 * y1 - y2 - 3 * y0;
        let c = y0;

        let x = (26501365 - n0) / input.len();

        (a2 * x * x + b2 * x + 2 * c) / 2
    }
}

#[cfg(test)]
mod tests {
    use crate::day21::{Day21, reachable_num, reachable_infinite_num};
    use crate::day::Day;

    use indoc::indoc;

    const INPUT: &str = indoc! {
        "...........
         .....###.#.
         .###.##..#.
         ..#.#...#..
         ....#.#....
         .##..S####.
         .##..#...#.
         .......##..
         .##.#.####.
         .##..##.##.
         ..........."};

    #[test]
    fn test21_1() {
        let day = Day21 {};
        assert_eq!(reachable_num(&day.parse_input(INPUT.to_string()), 6), 16);
    }

    #[test]
    fn test21_2() {
        let day = Day21 {};
        assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 6), 16);
        assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 10), 50);
        assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 50), 1594);
        assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 100), 6536);
        // assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 500), 167004);
        // assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 1000), 668697);
        // assert_eq!(reachable_infinite_num(&day.parse_input2(INPUT.to_string()), 5000), 16733044);
    }
}
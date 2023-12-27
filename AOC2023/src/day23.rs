use std::collections::HashSet;
use itertools::Itertools;

use crate::day::{Day, DayNum};

type Point = (i32, i32);

fn valid_point(i: i32, j: i32, map: &Vec<Vec<char>>) -> bool {
    let m = map.len() as i32;
    let n = map[0].len() as i32;

    0 <= i && i < m && 0 <= j && j < n && map[i as usize][j as usize] != '#'
}

fn find_paths(
    start: Point,
    end: Point,
    map: &Vec<Vec<char>>,
) -> Vec<HashSet<Point>> {
    let mut paths = vec![(start, HashSet::new())];
    let mut finished_paths = Vec::new();
    let mut max_len = 0;

    while !paths.is_empty() {
        let (current, mut visited) = paths.pop().unwrap();

        visited.insert(current);

        if current == end && visited.len() - 1 > max_len {
            finished_paths.push(visited.clone());
            max_len = visited.len() - 1;
            // println!("{max_len}");
        }

        let (i, j) = current;

        paths.extend(match map[i as usize][j as usize] {
            '>' => vec![(0, 1)],
            '<' => vec![(0, -1)],
            '^' => vec![(-1, 0)],
            'v' => vec![(1, 0)],
            '.' => vec![(0, 1), (0, -1), (-1, 0), (1, 0)],
            _ => unimplemented!(),
        }.into_iter()
            .filter(|&(di, dj)| valid_point(i + di, j + dj, map) && !visited.contains(&(i + di, j + dj)))
            .map(|(di, dj)| ((i + di, j + dj), visited.clone())));
    }

    finished_paths
}

fn get_longest_path(map: Vec<Vec<char>>) -> usize {
    let start = (0, 1);
    let end = (map.len() as i32 - 1, map[0].len() as i32 - 2);

    find_paths(start, end, &map).into_iter()
        .map(|p| p.len())
        .sorted()
        .last()
        .unwrap() - 1
}

pub struct Day23 {}

impl Day for Day23 {
    type Input = Vec<Vec<char>>;
    type Result = usize;

    fn day(&self) -> DayNum { 23 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars().collect()).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        get_longest_path(input)
    }

    fn parse_input2(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars()
            .map(|c| if c != '#' { '.' } else { '#' })
            .collect()).collect()
    }

    // 6490
    fn solve2(&self, input: Self::Input) -> Self::Result {
        get_longest_path(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::day23::Day23;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "#.#####################
         #.......#########...###
         #######.#########.#.###
         ###.....#.>.>.###.#.###
         ###v#####.#v#.###.#.###
         ###.>...#.#.#.....#...#
         ###v###.#.#.#########.#
         ###...#.#.#.......#...#
         #####.#.#.#######.#.###
         #.....#.#.#.......#...#
         #.#####.#.#.#########v#
         #.#...#...#...###...>.#
         #.#.#v#######v###.###v#
         #...#.>.#...>.>.#.###.#
         #####v#.#.###v#.#.###.#
         #.....#...#...#.#.#...#
         #.#########.###.#.#.###
         #...###...#...#...#.###
         ###.###.#.###v#####v###
         #...#...#.#.>.>.#.>.###
         #.###.###.#.###.#.#v###
         #.....###...###...#...#
         #####################.#"};

    const INPUT2: &str = indoc! {
        "#.###
         #...#
         #.###
         #...#
         ###.#"};

    #[test]
    fn test23_1() {
        let day = Day23 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 94);
        assert_eq!(day.solve1(day.parse_input(INPUT2.to_string())), 6);
    }

    #[test]
    fn test23_2() {
        let day = Day23 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 154);
        assert_eq!(day.solve2(day.parse_input2(INPUT2.to_string())), 6);
    }
}
use itertools::Itertools;
use std::collections::HashSet;

use crate::day::{Day, DayNum};
use crate::day16::Direction::{Up, Down, Left, Right};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn flow_light(grid: &Vec<Vec<char>>, start: (isize, isize, Direction)) -> HashSet<(isize, isize, Direction)> {
    let (m, n) = (grid.len() as isize, grid[0].len() as isize);
    let mut visited = HashSet::new();
    let mut dfs = vec![start];

    while !dfs.is_empty() {
        let (i, j, d): (isize, isize, Direction) = dfs.pop().unwrap();
        if 0 <= i && i < m && 0 <= j && j < n && !visited.contains(&(i, j, d)) {
            visited.insert((i, j, d));
            match (grid[i as usize][j as usize], d) {
                ('\\', Up) => dfs.push((i, j - 1, Left)),
                ('\\', Down) => dfs.push((i, j + 1, Right)),
                ('\\', Left) => dfs.push((i - 1, j, Up)),
                ('\\', Right) => dfs.push((i + 1, j, Down)),

                ('/', Up) => dfs.push((i, j + 1, Right)),
                ('/', Down) => dfs.push((i, j - 1, Left)),
                ('/', Left) => dfs.push((i + 1, j, Down)),
                ('/', Right) => dfs.push((i - 1, j, Up)),

                ('|', Left) => {
                    dfs.push((i + 1, j, Down));
                    dfs.push((i - 1, j, Up))
                }
                ('|', Right) => {
                    dfs.push((i + 1, j, Down));
                    dfs.push((i - 1, j, Up))
                }

                ('-', Up) => {
                    dfs.push((i, j - 1, Left));
                    dfs.push((i, j + 1, Right))
                }
                ('-', Down) => {
                    dfs.push((i, j - 1, Left));
                    dfs.push((i, j + 1, Right))
                }

                (_, Up) => dfs.push((i - 1, j, Up)),
                (_, Down) => dfs.push((i + 1, j, Down)),
                (_, Left) => dfs.push((i, j - 1, Left)),
                (_, Right) => dfs.push((i, j + 1, Right)),
            }
        }
    }


    visited
}

fn get_energized(grid: &Vec<Vec<char>>, start: (isize, isize, Direction)) -> usize {
    flow_light(grid, start).into_iter()
        .map(|(i, j, _)| (i, j))
        .collect::<HashSet<(isize, isize)>>()
        .iter()
        .count()
}

pub struct Day16 {}

impl Day for Day16 {
    type Input = Vec<Vec<char>>;
    type Result = usize;

    fn day(&self) -> DayNum { 16 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars().collect()).collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        get_energized(&input, (0, 0, Right))
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let (m, n) = (input.len() as isize, input[0].len() as isize);
        let mut energized_variants = Vec::new();
        for i in 0..m {
            energized_variants.push(get_energized(&input, (i, 0, Right)));
            energized_variants.push(get_energized(&input, (i, n - 1, Left)));
        }
        for j in 0..n {
            energized_variants.push(get_energized(&input, (0, j, Down)));
            energized_variants.push(get_energized(&input, (m - 1, j, Up)));
        }

        *energized_variants.iter().max().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::day16::Day16;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        ".|...\\....
         |.-.\\.....
         .....|-...
         ........|.
         ..........
         .........\\
         ..../.\\\\..
         .-.-/..|..
         .|....-|.\\
         ..//.|...."};

    #[test]
    fn test16_1() {
        let day = Day16 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 46);
    }

    #[test]
    fn test16_2() {
        let day = Day16 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 51);
    }
}
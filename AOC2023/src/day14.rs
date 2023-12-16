use itertools::Itertools;
use std::collections::{VecDeque, HashMap};

use crate::utils::{rotate_left, transpose};
use crate::day::{Day, DayNum};

fn move_rocks(old_rocks: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut rocks = old_rocks.clone();
    for row in rocks.iter_mut() {
        let mut free_indices = VecDeque::new();
        for c in row.iter_mut() {
            if *c == '.' {
                free_indices.push_back(c);
            } else if *c == 'O' {
                *c = '.';
                free_indices.push_back(c);
                *free_indices.pop_front().unwrap() = 'O'
            } else if *c == '#' {
                free_indices = VecDeque::new();
            }
        }
    }

    rocks
}

fn get_load(rocks: Vec<Vec<char>>) -> usize {
    let rows = rocks.len();
    rocks.into_iter()
        .enumerate()
        .map(|(i, r)| r.into_iter()
            .filter(|c| *c == 'O')
            .count() * (rows - i))
        .sum()
}

fn cycle(rocks: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let t = transpose(rocks);
    let north = move_rocks(t);
    let west = move_rocks(rotate_left(north));
    let south = move_rocks(rotate_left(west));
    let east = move_rocks(rotate_left(south));
    transpose(rotate_left(east))
}

pub struct Day14 {}

impl Day for Day14 {
    type Input = Vec<Vec<char>>;
    type Result = usize;

    fn day(&self) -> DayNum { 14 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars().collect()).collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let transposed = transpose(input);
        let moved_rocks = move_rocks(transposed);
        let original_orientation = transpose(moved_rocks);
        get_load(original_orientation)
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let cycles = 1000000000;
        let mut map = HashMap::new();
        let mut current = input.clone();

        map.insert(current.clone(), 0);
        for i in 1..cycles + 1 {
            let next = cycle(current);

            match map.get(&next) {
                None => map.insert(next.clone(), i),
                Some(v) => {
                    let target_index = v + (cycles - v) % (i - v);
                    current = map.into_iter()
                        .find_map(|(key, val)|
                            if val == target_index { Some(key) } else { None })
                        .unwrap();
                    break;
                }
            };

            current = next
        }

        get_load(current)
    }
}

#[cfg(test)]
mod tests {
    use crate::day14::Day14;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "O....#....
         O.OO#....#
         .....##...
         OO.#O....O
         .O.....O#.
         O.#..O.#.#
         ..O..#O..O
         .......O..
         #....###..
         #OO..#...."};

    #[test]
    fn test14_1() {
        let day = Day14 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 136);
    }

    #[test]
    fn test14_2() {
        let day = Day14 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 64);
    }
}
use itertools::Itertools;
use crate::day::{Day, DayNum};
use crate::utils::{is_palindrome, transpose};


fn row_reflections_prefix(block: &Vec<Vec<char>>) -> Vec<usize> {
    let mut reflections = Vec::new();
    for i in 1..(block.len() + 1) / 2 {
        if is_palindrome(&block[0..2 * i].iter().collect_vec()) {
            reflections.push(i);
        }
    }

    reflections
}

fn get_row_reflections(block: &Vec<Vec<char>>) -> Vec<usize> {
    let mut prefix = row_reflections_prefix(block);
    let block_rev = block.iter().cloned().rev().collect_vec();
    let suffix = row_reflections_prefix(&block_rev);
    prefix.extend(suffix.into_iter().map(|rows| block.len() - rows));

    prefix
}

fn get_block_values(block: Vec<Vec<char>>) -> Vec<usize> {
    let mut vals = Vec::new();
    vals.extend(get_row_reflections(&block).into_iter().map(|r| r * 100));
    let transposed = transpose(block);
    vals.extend(get_row_reflections(&transposed).into_iter());

    vals
}

fn block_modifications(block: Vec<Vec<char>>) -> Vec<Vec<Vec<char>>> {
    let mut new_blocks = Vec::new();
    for i in 0..block.len() {
        for j in 0..block[0].len() {
            let mut new_block = block.clone();
            match new_block[i][j] {
                '.' => new_block[i][j] = '#',
                '#' => new_block[i][j] = '.',
                _ => unimplemented!()
            }
            new_blocks.push(new_block);
        }
    }

    new_blocks
}

fn get_block_modified_value(block: Vec<Vec<char>>) -> usize {
    let base_valued = get_block_values(block.clone());

    for modified in block_modifications(block) {
        let new_vals = get_block_values(modified)
            .into_iter()
            .filter(|x| !base_valued.contains(x))
            .collect_vec();

        if !new_vals.is_empty() {
            return new_vals.iter().sum();
        }
    }

    unimplemented!()
}

pub struct Day13 {}

impl Day for Day13 {
    type Input = Vec<Vec<Vec<char>>>;
    type Result = usize;

    fn day(&self) -> DayNum { 13 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split("\n\n")
            .map(|block| block.split('\n').map(|l| l.chars().collect()).collect())
            .collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.into_iter().map(get_block_values).map(|vs| vs.iter().sum::<usize>()).sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        input.into_iter().map(get_block_modified_value).sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day13::Day13;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "#.##..##.
         ..#.##.#.
         ##......#
         ##......#
         ..#.##.#.
         ..##..##.
         #.#.##.#.

         #...##..#
         #....#..#
         ..##..###
         #####.##.
         #####.##.
         ..##..###
         #....#..#"};

    #[test]
    fn test13_1() {
        let day = Day13 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 405);
    }

    #[test]
    fn test13_2() {
        let day = Day13 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 400);
    }
}
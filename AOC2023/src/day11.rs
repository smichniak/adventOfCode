use itertools::Itertools;
use num::abs;

use crate::day::{Day, DayNum};
use crate::utils::transpose;

fn get_empty_rows(galaxies: &Vec<Vec<char>>) -> Vec<isize> {
    galaxies.iter()
        .scan(0, |empty, row|
            {
                if !row.contains(&'#') {
                    *empty += 1;
                }
                Some(*empty)
            })
        .collect_vec()
}

fn get_galaxy_coordinates(
    galaxies: Vec<Vec<char>>,
    empty_rows: Vec<isize>,
    empty_columns: Vec<isize>,
    expansion: isize,
) -> Vec<(isize, isize)> {
    galaxies.into_iter()
        .enumerate()
        .map(|(i, row)| row.into_iter()
            .positions(|x| x == '#')
            .map(|j| (i as isize + empty_rows[i] * (expansion - 1), j as isize + empty_columns[j] * (expansion - 1)))
            .collect_vec()
        )
        .concat()
}

fn find_distances(coordinates: Vec<(isize, isize)>) -> isize {
    let mut sum = 0;
    for i in 0..coordinates.len() {
        let (x1, y1) = coordinates[i];
        for j in i + 1..coordinates.len() {
            let (x2, y2) = coordinates[j];
            sum += abs(x1 - x2) + abs(y1 - y2)
        }
    }

    sum
}

fn galaxy_distances(galaxies: Vec<Vec<char>>, expansion: isize) -> isize {
    let empty_rows = get_empty_rows(&galaxies);
    let transposed = transpose(galaxies);
    let empty_columns = get_empty_rows(&transposed);
    let coordinates = get_galaxy_coordinates(transposed, empty_columns, empty_rows, expansion);

    find_distances(coordinates)
}

pub struct Day11 {}

impl Day for Day11 {
    type Input = Vec<Vec<char>>;
    type Result = isize;

    fn day(&self) -> DayNum { 11 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| l.chars().collect()).collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        galaxy_distances(input, 2)
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        galaxy_distances(input, 1000000)
    }
}

#[cfg(test)]
mod tests {
    use crate::day11::Day11;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "...#......
         .......#..
         #.........
         ..........
         ......#...
         .#........
         .........#
         ..........
         .......#..
         #...#....."};

    #[test]
    fn test11_1() {
        let day = Day11 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 374);
    }

    #[test]
    fn test11_2() {
        let day = Day11 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 82000210);
    }
}
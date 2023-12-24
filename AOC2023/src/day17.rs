use itertools::Itertools;
use std::collections::{HashMap, BinaryHeap};
use std::cmp::Reverse;

use crate::utils::{Direction, turn_left, turn_right, move_straight};
use crate::utils::Direction::Right;
use crate::day::{Day, DayNum};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Block {
    i: isize,
    j: isize,
    d: Direction,
    straight: u32,
}

fn valid_points(i: isize, j: isize, m: isize, n: isize) -> bool {
    0 <= i && i < m && 0 <= j && j < n
}

fn dijkstra_shortest(heat_losses: Vec<Vec<u32>>, min_straight: u32, max_straight: u32) -> u32 {
    let (m, n) = (heat_losses.len() as isize, heat_losses[0].len() as isize);
    let mut shortest_paths: HashMap<Block, u32> = HashMap::new();
    let mut dijkstra_queue = BinaryHeap::new();

    dijkstra_queue.push(Reverse((0, Block { i: 0, j: 0, d: Right, straight: 0 })));

    while !dijkstra_queue.is_empty() {
        let Reverse((loss, block)) = dijkstra_queue.pop().unwrap();
        let Block { i, j, d, straight } = block;

        if !shortest_paths.contains_key(&block) {
            shortest_paths.insert(block, loss);

            if straight >= min_straight {
                let (ir, jr, dr) = turn_right(i, j, d);
                if valid_points(ir, jr, m, n) {
                    let new_loss = loss + heat_losses[ir as usize][jr as usize];
                    dijkstra_queue.push(Reverse((new_loss, Block { i: ir, j: jr, d: dr, straight: 1 })));
                }

                let (il, jl, dl) = turn_left(i, j, d);
                if valid_points(il, jl, m, n) {
                    let new_loss = loss + heat_losses[il as usize][jl as usize];
                    dijkstra_queue.push(Reverse((new_loss, Block { i: il, j: jl, d: dl, straight: 1 })));
                }
            }

            if straight < max_straight {
                let (is, js, ds) = move_straight(i, j, d);
                if valid_points(is, js, m, n) {
                    let new_loss = loss + heat_losses[is as usize][js as usize];
                    dijkstra_queue.push(Reverse((new_loss, Block { i: is, j: js, d: ds, straight: straight + 1 })));
                }
            }
        }
    }

    shortest_paths.iter()
        .filter(|(&Block { i, j, d: _, straight }, _)|
            i == m - 1 && j == n - 1 && straight >= min_straight)
        .map(|(_, &v)| v)
        .min()
        .unwrap()
}

pub struct Day17 {}

impl Day for Day17 {
    type Input = Vec<Vec<u32>>;
    type Result = u32;

    fn day(&self) -> DayNum { 17 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n')
            .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
            .collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        dijkstra_shortest(input, 0, 3)
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        dijkstra_shortest(input, 4, 10)
    }
}

#[cfg(test)]
mod tests {
    use crate::day17::Day17;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "2413432311323
         3215453535623
         3255245654254
         3446585845452
         4546657867536
         1438598798454
         4457876987766
         3637877979653
         4654967986887
         4564679986453
         1224686865563
         2546548887735
         4322674655533"};

    const INPUT2: &str = indoc! {
        "111111111111
         999999999991
         999999999991
         999999999991
         999999999991"};

    #[test]
    fn test17_1() {
        let day = Day17 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 102);
    }

    #[test]
    fn test17_2() {
        let day = Day17 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 94);
        assert_eq!(day.solve2(day.parse_input2(INPUT2.to_string())), 71);
    }
}
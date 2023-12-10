use itertools::Itertools;
use num::range;
use std::cmp::min;

use crate::day::{Day, DayNum};
use crate::day10::Pipe::{Ground, Horizontal, NE, NW, SW, SE, Start, Vertical};
use crate::day10::PlaceType::{Inside, Outside, Unknown};

#[derive(PartialEq, Eq)]
pub enum Pipe {
    Vertical,
    Horizontal,
    NE,
    NW,
    SW,
    SE,
    Ground,
    Start,
}

impl From<char> for Pipe {
    fn from(c: char) -> Self {
        match c {
            '|' => Vertical,
            '-' => Horizontal,
            'L' => NE,
            'J' => NW,
            'F' => SW,
            '7' => SE,
            '.' => Ground,
            'S' => Start,
            _ => unimplemented!(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
enum PlaceType {
    Inside,
    Outside,
    Unknown,
}

type Coordinate = (i32, i32);

impl Pipe {
    fn get_adjacent(&self) -> Vec<Coordinate> {
        match self {
            Vertical => vec![(0, -1), (0, 1)],
            Horizontal => vec![(-1, 0), (1, 0)],
            NE => vec![(0, -1), (1, 0)],
            NW => vec![(0, -1), (-1, 0)],
            SW => vec![(0, 1), (1, 0)],
            SE => vec![(0, 1), (-1, 0)],
            Ground => vec![],
            Start => vec![Vertical.get_adjacent(), Horizontal.get_adjacent()].concat(),
        }
    }
}

fn get_start(map: &Vec<Vec<Pipe>>) -> Coordinate {
    map.iter()
        .enumerate()
        .filter_map(|(i, l)| l.iter()
            .map(|x| (i, x))
            .find_position(|(_, x)| **x == Start))
        .map(|(i, (j, _))| (i as i32, j as i32))
        .nth(0)
        .unwrap()
}

fn get_path(
    src: Coordinate,
    dst: Coordinate,
    prev: Coordinate,
    map: &Vec<Vec<Pipe>>,
) -> Vec<Coordinate> {
    if src == dst {
        return vec![dst];
    }

    let (is, js) = src;
    for (di, dj) in map[js as usize][is as usize].get_adjacent() {
        let new_coord = (is + di, js + dj);
        if new_coord != prev {
            let mut v = get_path(new_coord, dst, src, map);
            v.push(src);
            return v;
        }
    }

    unimplemented!() // Never used
}

fn path_from_start(start: Coordinate, map: &Vec<Vec<Pipe>>) -> Vec<Coordinate> {
    let (is, js) = start;

    for (di, dj) in Start.get_adjacent() {
        if 0 <= is + di && is + di < map[0].len() as i32 &&
            0 <= js + dj && js + dj < map.len() as i32 &&
            map[(js + dj) as usize][(is + di) as usize].get_adjacent().contains(&(-di, -dj))
        {
            let src = ((is + di), (js + dj));
            return get_path(src, (is, js), (is, js), map);
        }
    }

    unimplemented!() // Never happens, path always found in the loop
}

fn fill_places(point: Coordinate, m: i32, n: i32, places: &mut Vec<Vec<PlaceType>>) {
    let (i, j) = point;
    if places[j as usize][i as usize] == Unknown {
        return;
    }

    for (di, dj) in Start.get_adjacent() {
        if set_place(i + di, j + dj, places[j as usize][i as usize].clone(), places) {
            fill_places((i + di, j + dj), m, n, places);
        }
    }
}

fn set_place(i: i32, j: i32, val: PlaceType, places: &mut Vec<Vec<PlaceType>>) -> bool {
    if 0 <= i && i < places[0].len() as i32 &&
        0 <= j && j < places.len() as i32 &&
        places[j as usize][i as usize] == Unknown
    {
        places[j as usize][i as usize] = val;
        return true;
    }

    false
}

pub struct Day10 {}

impl Day for Day10 {
    type Input = Vec<Vec<Pipe>>;
    type Result = usize;

    fn day(&self) -> DayNum { 10 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n')
            .map(|l| l.chars().map(|c| Pipe::from(c)).collect_vec())
            .collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        path_from_start(get_start(&input), &input).len() / 2
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let start = get_start(&input);
        let mut path = path_from_start(start, &input);
        let (m, n) = (input.len() as i32, input[0].len() as i32);

        let mut places = input.into_iter()
            .map(|v| v.iter().map(|_| Unknown).collect_vec())
            .collect_vec();

        for (i, j) in &path {
            places[*j as usize][*i as usize] = Inside;
        }

        path.push(path[0]);
        for w in path.windows(2) {
            let ((i1, j1), (i2, j2)) = (w[0], w[1]);
            let diff = (i2 - i1, j2 - j1);

            let (dri, drj) = match diff {
                (0, -1) => (1, 0),
                (1, 0) => (0, 1),
                (0, 1) => (-1, 0),
                (-1, 0) => (0, -1),
                _ => unimplemented!()
            };

            set_place(i1 + dri, j1 + drj, Inside, &mut places);
            set_place(i1 - dri, j1 - drj, Outside, &mut places);
            set_place(i2 + dri, j2 + drj, Inside, &mut places);
            set_place(i2 - dri, j2 - drj, Outside, &mut places);
        }

        for j in range(0, m) {
            for i in range(0, n) {
                fill_places((i, j), m, n, &mut places);
            }
        }

        for (i, j) in &path {
            places[*j as usize][*i as usize] = Unknown;
        }

        let v = places.into_iter()
            .map(|row| {
                let p_count = row.into_iter().counts();
                (*p_count.get(&Inside).unwrap_or(&0), *p_count.get(&Outside).unwrap_or(&0))
                // row.into_iter().filter(|x| *x == Inside).count()
            })
            .reduce(|(i1, o1), (i2, o2)| (i1 + i2, o1 + o2))
            .unwrap();


        min(v.0, v.1) // Just a heuristic
    }
}

#[cfg(test)]
mod tests {
    use crate::day10::Day10;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "7-F7-
         .FJ|7
         SJLL7
         |F--J
         LJ.LJ"};

    const INPUT2: &str = indoc! {
        ".F----7F7F7F7F-7....
         .|F--7||||||||FJ....
         .||.FJ||||||||L7....
         FJL7L7LJLJ||LJ.L-7..
         L--J.L7...LJS7F-7L7.
         ....F-J..F7FJ|L7L7L7
         ....L7.F7||L7|.L7L7|
         .....|FJLJ|FJ|F7|.LJ
         ....FJL-7.||.||||...
         ....L---J.LJ.LJLJ..."};

    #[test]
    fn test10_1() {
        let day = Day10 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 8);
    }

    #[test]
    fn test10_2() {
        let day = Day10 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT2.to_string())), 8);
    }
}
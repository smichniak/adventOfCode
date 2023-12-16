use itertools::Itertools;
use nom::AsChar;


use crate::day15::Operation::{Add, Remove};
use crate::day::{Day, DayNum};

enum Operation {
    Remove,
    Add,
}

struct Instruction {
    label: Vec<u8>,
    operation: Operation,
    power: u32,
}

impl From<Vec<u8>> for Instruction {
    fn from(value: Vec<u8>) -> Self {
        match value[value.len() - 1] {
            b'-' => Self {
                label: value[..value.len() - 1].to_vec(),
                operation: Remove,
                power: 0,
            },
            num => Self {
                label: value[..value.len() - 2].to_vec(),
                operation: Add,
                power: num.as_char().to_digit(10).unwrap(),
            },
        }
    }
}

fn hash(bytes: Vec<u8>) -> u32 {
    bytes.iter().fold(0, |acc, &x| (17 * (acc + x as u32)) % 256)
}

fn get_focusing_power(boxes: Vec<Vec<(Vec<u8>, u32)>>) -> u32 {
    boxes.into_iter()
        .enumerate()
        .map(|(box_num, lenses)|
            lenses.into_iter().enumerate().map(|(lens_num, (_, power))|
                (box_num as u32 + 1) * (lens_num as u32 + 1) * power).collect_vec()
        )
        .concat()
        .iter()
        .sum()
}

pub struct Day15 {}

impl Day for Day15 {
    type Input = Vec<Vec<u8>>;
    type Result = u32;

    fn day(&self) -> DayNum { 15 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split(',').map(|l| l.as_bytes().to_vec()).collect_vec()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.into_iter().map(hash).sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let instructions = input.into_iter().map(|s| Instruction::from(s));
        let mut boxes = vec![Vec::<(Vec<u8>, u32)>::new(); 256];

        for Instruction { label, operation, power } in instructions {
            let box_num = hash(label.clone()) as usize;
            match operation {
                Remove => boxes[box_num] = boxes[box_num].clone().into_iter().filter(|(l, _)| *l != label).collect_vec(),
                Add => {
                    let index = boxes[box_num].clone().into_iter().find_position(|(l, _)| *l == label);
                    boxes[box_num].push((label, power));
                    index.and_then(|(i, _)| Some(boxes[box_num].swap_remove(i)));
                }
            }
        }


        get_focusing_power(boxes)
    }
}

#[cfg(test)]
mod tests {
    use crate::day15::Day15;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"};

    #[test]
    fn test15_1() {
        let day = Day15 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 1320);
    }

    #[test]
    fn test15_2() {
        let day = Day15 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 145);
    }
}
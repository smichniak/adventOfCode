use crate::day::{Day, DayNum};

pub struct Day1 {}

impl Day for Day1 {
    type Input = String;
    type Result = u32;

    fn day(&self) -> DayNum { 1 }

    fn parse_input(&self, _file_input: String) -> Self::Input {
        unimplemented!()
    }

    fn solve1(&self, _input: Self::Input) -> Self::Result {
        unimplemented!()
    }
    fn solve2(&self, _input: Self::Input) -> Self::Result {
        unimplemented!()
    }
}
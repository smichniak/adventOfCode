use crate::day::{Day, DayNum};

pub struct Day01 {}

impl Day for Day01 {
    type Input = String;
    type Result = u32;

    fn day(&self) -> DayNum { 01 }

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
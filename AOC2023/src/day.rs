use std::fmt::Display;
use std::path::Path;
use std::fs::read_to_string;
use std::rc::Rc;

pub type DayNum = u8;


pub trait Day {
    type Input;
    type Result: Display;

    fn day(&self) -> DayNum;

    fn load_input(&self) -> String {
        let input_filename = format!("./input/day{:02}.in", self.day());
        let input_filepath = Path::new(input_filename.as_str());
        read_to_string(input_filepath).unwrap_or_else(|err| {
            eprintln!("Input file reading error: {err}");
            std::process::exit(1);
        })
    }

    fn parse_input(&self, file_input: String) -> Self::Input;

    fn solve1(&self, input: Self::Input) -> Self::Result;

    fn parse_input2(&self, file_input: String) -> Self::Input { self.parse_input(file_input) }

    fn solve2(&self, input: Self::Input) -> Self::Result;
}

pub struct Runner {
    day: DayNum,
    pub part1: Box<dyn Fn() -> String>,
    pub part2: Box<dyn Fn() -> String>,
}


impl Runner {
    pub fn day(&self) -> DayNum {
        self.day
    }
    pub fn new<T: 'static, R: Display + 'static>(day: Rc<dyn Day<Input=T, Result=R>>) -> Self {
        let d1 = day.clone();
        let d2 = day.clone();
        let part1 = Box::new(move || { format!("{}", d1.solve1(d1.parse_input(d1.load_input()))) });
        let part2 = Box::new(move || { format!("{}", d2.solve2(d2.parse_input2(d2.load_input()))) });
        Runner { day: day.day(), part1, part2 }
    }
}
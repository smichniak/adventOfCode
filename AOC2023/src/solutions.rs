use std::collections::BTreeMap;
use std::rc::Rc;
use crate::day::{DayNum, Runner};
use crate::day01::Day01;
use crate::day02::Day02;
use crate::day03::Day03;
use crate::day04::Day04;
use crate::day05::Day05;
use crate::day06::Day06;
use crate::day07::Day07;
use crate::day08::Day08;
use crate::day09::Day09;
use crate::day10::Day10;
use crate::day11::Day11;
use crate::day12::Day12;
use crate::day13::Day13;
use crate::day14::Day14;
use crate::day15::Day15;
use crate::day16::Day16;
use crate::day17::Day17;
use crate::day18::Day18;
use crate::day19::Day19;
use crate::day20::Day20;
use crate::day21::Day21;
use crate::day22::Day22;
use crate::day23::Day23;
use crate::day24::Day24;
use crate::day25::Day25;

pub type RunnerMap = BTreeMap<DayNum, Runner>;

pub fn get_runners() -> RunnerMap {
    let mut map = BTreeMap::new();

    let runners = [
        Runner::new(Rc::new(Day01 {})),
        Runner::new(Rc::new(Day02 {})),
        Runner::new(Rc::new(Day03 {})),
        Runner::new(Rc::new(Day04 {})),
        Runner::new(Rc::new(Day05 {})),
        Runner::new(Rc::new(Day06 {})),
        Runner::new(Rc::new(Day07 {})),
        Runner::new(Rc::new(Day08 {})),
        Runner::new(Rc::new(Day09 {})),
        Runner::new(Rc::new(Day10 {})),
        Runner::new(Rc::new(Day11 {})),
        Runner::new(Rc::new(Day12 {})),
        Runner::new(Rc::new(Day13 {})),
        Runner::new(Rc::new(Day14 {})),
        Runner::new(Rc::new(Day15 {})),
        Runner::new(Rc::new(Day16 {})),
        Runner::new(Rc::new(Day17 {})),
        Runner::new(Rc::new(Day18 {})),
        Runner::new(Rc::new(Day19 {})),
        Runner::new(Rc::new(Day20 {})),
        Runner::new(Rc::new(Day21 {})),
        Runner::new(Rc::new(Day22 {})),
        Runner::new(Rc::new(Day23 {})),
        Runner::new(Rc::new(Day24 {})),
        Runner::new(Rc::new(Day25 {})),
    ];

    for r in runners {
        map.insert(r.day(), r);
    }
    map
}
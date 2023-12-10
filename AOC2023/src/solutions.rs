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

pub type RunnerMap = BTreeMap<DayNum, Runner>;

pub fn get_runners() -> RunnerMap {
    let mut map = BTreeMap::new();

    let r1 = Runner::new(Rc::new(Day01 {}));
    let r2 = Runner::new(Rc::new(Day02 {}));
    let r3 = Runner::new(Rc::new(Day03 {}));
    let r4 = Runner::new(Rc::new(Day04 {}));
    let r5 = Runner::new(Rc::new(Day05 {}));
    let r6 = Runner::new(Rc::new(Day06 {}));
    let r7 = Runner::new(Rc::new(Day07 {}));
    let r8 = Runner::new(Rc::new(Day08 {}));
    let r9 = Runner::new(Rc::new(Day09 {}));
    let r10 = Runner::new(Rc::new(Day10 {}));

    let runners = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10];

    for r in runners {
        map.insert(r.day(), r);
    }
    map
}
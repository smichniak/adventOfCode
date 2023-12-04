use std::collections::BTreeMap;
use std::rc::Rc;
use crate::day::{DayNum, Runner};
use crate::day01::Day01;
use crate::day02::Day02;
use crate::day03::Day03;
use crate::day04::Day04;

pub type RunnerMap = BTreeMap<DayNum, Runner>;

pub fn get_runners() -> RunnerMap {
    let mut map = BTreeMap::new();

    let r1 = Runner::new(Rc::new(Day01 {}));
    let r2 = Runner::new(Rc::new(Day02 {}));
    let r3 = Runner::new(Rc::new(Day03 {}));
    let r4 = Runner::new(Rc::new(Day04 {}));

    let runners = [r1, r2, r3, r4];

    for r in runners {
        map.insert(r.day(), r);
    }
    map
}



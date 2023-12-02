use std::collections::BTreeMap;
use std::rc::Rc;
use crate::day::{DayNum, Runner};
use crate::day01::Day01;
use crate::day02::Day02;

pub type RunnerMap = BTreeMap<DayNum, Runner>;

pub fn get_runners() -> RunnerMap {
    let mut map = BTreeMap::new();

    let r1 = Runner::new(Rc::new(Day01 {}));
    let r2 = Runner::new(Rc::new(Day02 {}));

    let runners = [r1, r2];

    for r in runners {
        map.insert(r.day(), r);
    }
    map
}



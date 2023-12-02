use std::collections::BTreeMap;
use std::rc::Rc;
use crate::day::{DayNum, Runner};
use crate::day1::Day1;
use crate::day2::Day2;

pub type RunnerMap = BTreeMap<DayNum, Runner>;

pub fn get_runners() -> RunnerMap {
    let mut map = BTreeMap::new();

    let r1 = Runner::new(Rc::new(Day1 {}));
    let r2 = Runner::new(Rc::new(Day2 {}));

    let runners = [r1, r2];

    for r in runners {
        map.insert(r.day(), r);
    }
    map
}



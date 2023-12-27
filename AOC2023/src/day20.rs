use nom::error::ErrorKind;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, none_of};
use nom::sequence::separated_pair;
use nom::multi::{many1, separated_list1};

use std::collections::{HashMap, HashSet, VecDeque};
use num::integer::lcm;

use crate::day::{Day, DayNum};
use crate::utils::parse;
use crate::day20::ModuleType::{Broadcast, Conjunction, FlipFlop};

#[derive(Clone)]
enum ModuleType {
    FlipFlop,
    Conjunction,
    Broadcast,
}

#[derive(Clone)]
pub struct Module {
    name: String,
    module_type: ModuleType,
    destinations: HashSet<String>,
    state: HashMap<String, bool>,
}

fn parse_line(line: &str) -> Module {
    let module_spec_parser = many1::<_, _, (_, ErrorKind), _>(none_of(" "));
    let module_dst_parser = separated_list1(tag(", "), alpha1);
    let module_parser = separated_pair(module_spec_parser, tag(" -> "), module_dst_parser);
    let (module_spec, module_dst) = parse(module_parser, line);

    let module_type = match module_spec[0] {
        '%' => FlipFlop,
        '&' => Conjunction,
        _ => Broadcast,
    };

    let name = module_spec[1..].into_iter().collect();

    Module {
        name,
        module_type,
        destinations: module_dst.into_iter().map(|s| s.to_string()).collect(),
        state: HashMap::new(),
    }
}

fn initialise_module(modules: &HashMap<String, Module>, module: &mut Module) {
    if let FlipFlop = module.module_type {
        module.state = HashMap::from([("".to_string(), false)]);
    } else if let Conjunction = module.module_type {
        module.state = modules.clone()
            .values()
            .into_iter()
            .filter_map(|Module { name, module_type: _, destinations, state: _ }|
                if destinations.contains(module.name.as_str()) { Some((name.clone(), false)) } else { None })
            .collect();
    }
}

fn process_pulses(
    modules: &mut HashMap<String, Module>,
    pulses: &mut VecDeque<(String, String, bool)>,
    target_module: &String,
) -> (usize, usize, bool) {
    let mut low_pulses = 0;
    let mut high_pulses = 0;
    let mut got_high = false;

    while !pulses.is_empty() {
        let (src_name, dst_name, high) = pulses.pop_front().unwrap();
        if high {
            high_pulses += 1;
            if src_name == *target_module {
                got_high = true;
            };
        } else {
            low_pulses += 1;
        }

        modules.entry(dst_name.clone()).and_modify(|module|
            match module.module_type {
                FlipFlop => if !high {
                    module.state.insert("".to_string(), !module.state[""]);
                    module.destinations.iter().for_each(|dst|
                        pulses.push_back((dst_name.clone(), dst.clone(), module.state[""])));
                },
                Conjunction => {
                    module.state.insert(src_name, high);
                    let new_pulse = !module.state.clone().values().all(|&v| v);
                    module.destinations.iter().for_each(|dst|
                        pulses.push_back((dst_name.clone(), dst.clone(), new_pulse)));
                }
                Broadcast => module.destinations.iter().for_each(|dst|
                    pulses.push_back((dst_name.clone(), dst.clone(), high))),
            });
    }

    (low_pulses, high_pulses, got_high)
}

fn press_button(modules: &mut HashMap<String, Module>, target_module: &String) -> (usize, usize, bool) {
    let mut initial_pulse = VecDeque::from([("button".to_string(), "roadcaster".to_string(), false)]);
    process_pulses(modules, &mut initial_pulse, target_module)
}

fn get_presses(modules: &mut HashMap<String, Module>, name: &String) -> usize {
    let module_clone = modules.clone();
    for (_, m) in &mut *modules {
        initialise_module(&module_clone, m);
    }
    let mut presses = 1;
    while !press_button(modules, name).2 { presses += 1; }
    presses
}

pub struct Day20 {}

impl Day for Day20 {
    type Input = HashMap<String, Module>;
    type Result = usize;

    fn day(&self) -> DayNum { 20 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line)
            .map(|m| (m.name.clone(), m))
            .collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let mut modules = input.clone();
        modules.iter_mut().for_each(|(_, m)| initialise_module(&input, m));
        let mut low = 0;
        let mut high = 0;
        for _ in 0..1000 {
            let (new_low, new_high, _) = press_button(&mut modules, &"NO_MODULE".to_string());
            low += new_low;
            high += new_high;
        }

        low * high
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let mut modules = input.clone();

        let rx_src = modules.clone()
            .iter_mut()
            .filter_map(|(_, m)| m.destinations.take("rx").and(Some(m.name.clone())))
            .next()
            .unwrap();

        modules.clone()
            .iter_mut()
            .filter_map(|(_, m)| m.destinations.take(rx_src.as_str()).and(Some(m.name.clone())))
            .map(|name| get_presses(&mut modules, &name))
            .reduce(|acc, x| lcm(acc, x)).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::day20::Day20;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT1: &str = indoc! {
        "broadcaster -> a, b, c
         %a -> b
         %b -> c
         %c -> inv
         &inv -> a"};

    const INPUT2: &str = indoc! {
        "broadcaster -> a
         %a -> inv, con
         &inv -> b
         %b -> con
         &con -> output"};

    #[test]
    fn test20_1() {
        let day = Day20 {};
        assert_eq!(day.solve1(day.parse_input(INPUT1.to_string())), 32000000);
        assert_eq!(day.solve1(day.parse_input(INPUT2.to_string())), 11687500);
    }
}
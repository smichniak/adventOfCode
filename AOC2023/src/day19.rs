use nom::IResult;
use nom::error::ErrorKind;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha0, alpha1, u32 as nomU32};
use nom::sequence::{tuple, separated_pair, delimited, preceded, pair};
use nom::branch::alt;
use nom::multi::{separated_list1};

use itertools::Itertools;
use std::collections::HashMap;

use crate::day::{Day, DayNum};
use crate::day19::Result::{Accepted, Next, Rejected};
use crate::utils::parse;

#[derive(Clone)]
pub struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Result {
    Accepted,
    Rejected,
    Next(String),
}

impl From<&str> for Result {
    fn from(value: &str) -> Self {
        match value {
            "A" => Accepted,
            "R" => Rejected,
            s => Next(s.to_string()),
        }
    }
}

type Rule = Box<dyn Fn(&Part) -> Option<Result>>;

pub struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

fn get_rule(attribute: &str, cmp: &str, num: usize, result_str: &str) -> Rule {
    let result = Some(Result::from(result_str));
    let attribute_clone = attribute.clone().to_string();
    let cmp_clone = cmp.clone().to_string();

    let part_getter = move |part: &Part|
        match attribute_clone.as_str() {
            "x" => part.x,
            "m" => part.m,
            "a" => part.a,
            "s" => part.s,
            _ => unimplemented!(),
        };

    let comparator = move |n| match cmp_clone.as_str() {
        "<" => n < num,
        ">" => n > num,
        _ => unimplemented!(),
    };

    Box::new(move |part| {
        let part_num = part_getter(&part);
        if comparator(part_num) { result.clone() } else { None }
    })
}

fn parse_rule(input: &str) -> IResult<&str, Rule> {
    let condition = tuple((alpha1::<_, (_, ErrorKind)>, alt((tag("<"), tag(">"))), nomU32));
    let mut complex_rule = separated_pair(condition, tag(":"), alpha1);

    match complex_rule(input) {
        Ok((remaining, ((attribute, cmp, num), result))) =>
            Ok((remaining, get_rule(attribute, cmp, num as usize, result))),
        Err(_) => {
            let (remaining, last_rule) = alpha0(input)?;
            let result = Result::from(last_rule);
            Ok((remaining, Box::new(move |_| Some(result.clone()))))
        }
    }
}

fn parse_workflow(workflow_str: &str) -> Workflow {
    let rules = separated_list1(tag(","), parse_rule);
    let workflow = pair(alpha1, delimited(tag("{"), rules, tag("}")));

    let (name, rules) = parse(workflow, workflow_str);
    Workflow { name: name.to_string(), rules }
}


fn parse_part(part_str: &str) -> Part {
    let assign = preceded(take::<_, _, (_, ErrorKind)>(2usize), nomU32);
    let list = separated_list1(tag(","), assign);
    let bracketed = delimited(tag("{"), list, tag("}"));
    let nums = parse(bracketed, part_str);

    Part { x: nums[0] as usize, m: nums[1] as usize, a: nums[2] as usize, s: nums[3] as usize }
}

fn process_part(workflows: &HashMap<String, Workflow>, part: &Part, workflow_name: String) -> Result {
    let Workflow { name: _, rules } = &workflows[&workflow_name];

    match rules.iter().filter_map(|rule| rule(part)).next().unwrap() {
        Next(s) => process_part(workflows, part, s),
        r => r
    }
}

fn split_part(part1: &Part, part2: &Part) -> Vec<(Part, Part)> {
    let Part { x: x1, m: m1, a: a1, s: s1 } = *part1;
    let Part { x: x2, m: m2, a: a2, s: s2 } = *part2;

    let x_mid1 = Part { x: (x1 + x2) / 2, m: m1, a: a1, s: s1 };
    let x_mid2 = Part { x: (x1 + x2) / 2, m: m2, a: a2, s: s2 };

    let m_mid1 = Part { x: x1, m: (m1 + m2) / 2, a: a1, s: s1 };
    let m_mid2 = Part { x: x2, m: (m1 + m2) / 2, a: a2, s: s2 };

    let a_mid1 = Part { x: x1, m: m1, a: (a1 + a2) / 2, s: s1 };
    let a_mid2 = Part { x: x2, m: m2, a: (a1 + a2) / 2, s: s2 };

    let s_mid1 = Part { x: x1, m: m1, a: a1, s: (s1 + s2) / 2 };
    let s_mid2 = Part { x: x2, m: m2, a: a2, s: (s1 + s2) / 2 };

    vec![(x_mid1, x_mid2), (m_mid1, m_mid2), (a_mid1, a_mid2), (s_mid1, s_mid2)]
}

fn process_part_range(
    workflows: &HashMap<String, Workflow>,
    part_low: &Part,
    part_high: &Part,
    workflow_name: &String,
) -> Vec<(Part, Part)> {
    let Workflow { name: _, rules } = &workflows[workflow_name];

    for rule in rules {
        let result1 = rule(part_low);
        let result2 = rule(&Part { x: part_high.x - 1, m: part_high.m - 1, a: part_high.a - 1, s: part_high.s - 1 });

        if result1 == result2 {
            match result1 {
                None => continue,
                Some(Next(s)) => return process_part_range(workflows, part_low, part_high, &s),
                Some(Accepted) => return vec![(part_low.clone(), part_high.clone())],
                Some(Rejected) => return vec![],
            }
        } else {
            for (p2, p3) in split_part(part_low, part_high) {
                if rule(&p2) != result1 || rule(&p3) != result2 {
                    return vec![process_part_range(workflows, part_low, &p3, workflow_name),
                                process_part_range(workflows, &p2, part_high, workflow_name)].concat();
                }
            }
        }
    }

    unimplemented!()
}

pub struct Day19 {}

impl Day for Day19 {
    type Input = (HashMap<String, Workflow>, Vec<Part>);
    type Result = usize;

    fn day(&self) -> DayNum { 19 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        let mut split_input = file_input.split("\n\n");
        let workflows = split_input.next()
            .unwrap()
            .split('\n')
            .map(parse_workflow)
            .map(|Workflow { name, rules }| (name.clone(), Workflow { name, rules }))
            .collect();


        let parts = split_input.next()
            .unwrap()
            .split('\n')
            .map(parse_part)
            .collect_vec();

        (workflows, parts)
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let (workflows, parts) = input;

        parts.iter()
            .map(|p| (p, process_part(&workflows, p, "in".to_string())))
            .filter(|(_, r)| *r == Accepted)
            .map(|(&Part { x, m, a, s }, _)| x + m + a + s)
            .sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let (workflows, _) = input;
        let part_low = Part { x: 1, m: 1, a: 1, s: 1 };
        let part_high = Part { x: 4001, m: 4001, a: 4001, s: 4001 };

        process_part_range(&workflows, &part_low, &part_high, &"in".to_string()).into_iter()
            .map(|(p1, p2)| {
                let Part { x: x1, m: m1, a: a1, s: s1 } = p1;
                let Part { x: x2, m: m2, a: a2, s: s2 } = p2;
                (x2 - x1) * (m2 - m1) * (a2 - a1) * (s2 - s1)
            })
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day19::Day19;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "px{a<2006:qkq,m>2090:A,rfg}
         pv{a>1716:R,A}
         lnx{m>1548:A,A}
         rfg{s<537:gd,x>2440:R,A}
         qs{s>3448:A,lnx}
         qkq{x<1416:A,crn}
         crn{x>2662:A,R}
         in{s<1351:px,qqz}
         qqz{s>2770:qs,m<1801:hdj,R}
         gd{a>3333:R,R}
         hdj{m>838:A,pv}

         {x=787,m=2655,a=1222,s=2876}
         {x=1679,m=44,a=2067,s=496}
         {x=2036,m=264,a=79,s=2244}
         {x=2461,m=1339,a=466,s=291}
         {x=2127,m=1623,a=2188,s=1013}"};

    #[test]
    fn test19_1() {
        let day = Day19 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 19114);
    }

    #[test]
    fn test19_2() {
        let day = Day19 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 167409079868000);
    }
}
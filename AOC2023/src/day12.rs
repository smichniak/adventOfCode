use nom::multi::{many1, separated_list1};
use nom::bytes::complete::tag;
use nom::character::complete::{none_of, i32 as nomI32};
use nom::error::ErrorKind;
use nom::sequence::separated_pair;

use std::collections::HashMap;

use crate::day::{Day, DayNum};
use crate::utils::parse;

type Springs = (Vec<char>, Vec<i32>);

fn parse_line(line: &str) -> Springs {
    let nums = separated_list1(tag(","), nomI32::<_, (_, ErrorKind)>);
    let springs = separated_pair(many1(none_of(" ")), tag(" "), nums);

    parse(springs, line)
}


fn group_ways(s: Vec<char>, groups: Vec<i32>, memo: &mut HashMap<Springs, usize>) -> usize {
    if !memo.contains_key(&(s.clone(), groups.clone())) {
        if groups.is_empty() || (groups == vec![0]) {
            if s.contains(&'#') {
                memo.insert((s.clone(), groups.clone()), 0);
            } else {
                memo.insert((s.clone(), groups.clone()), 1);
            }
        } else if s.is_empty() {
            memo.insert((s.clone(), groups.clone()), 0);
        } else if groups[0] == 0 {
            match s[0] {
                '#' => memo.insert((s.clone(), groups.clone()), 0),
                _ => {
                    let new_v = group_ways(s.clone()[1..].to_vec(), groups.clone()[1..].to_vec(), memo);
                    memo.insert((s.clone(), groups.clone()), new_v)
                }
            };
        } else if s[0] == '#' {
            let new_s = s.clone()[1..].to_vec();
            let mut new_g = groups.clone();
            if new_g[0] > 0 {
                new_g[0] = -new_g[0] + 1;
            } else {
                new_g[0] += 1;
            }

            let new_v = group_ways(new_s, new_g, memo);
            memo.insert((s.clone(), groups.clone()), new_v);
        } else if s[0] == '.' {
            if groups[0] < 0 {
                memo.insert((s.clone(), groups.clone()), 0);
            } else {
                let new_v = group_ways(s.clone()[1..].to_vec(), groups.clone().to_vec(), memo);
                memo.insert((s.clone(), groups.clone()), new_v);
            }
        } else {
            let mut no_damage = s.clone().to_vec();
            no_damage[0] = '.';
            let mut damage = s.clone().to_vec();
            damage[0] = '#';

            let no_damage_ways = group_ways(no_damage, groups.clone().to_vec(), memo);
            let damage_ways = group_ways(damage, groups.clone().to_vec(), memo);
            memo.insert((s.clone(), groups.clone()), no_damage_ways + damage_ways);
        }
    };

    *memo.get(&(s, groups)).unwrap()
}

fn spring_ways(springs: Springs) -> usize {
    let (s, groups) = springs;
    group_ways(s, groups, &mut HashMap::new())
}

pub struct Day12 {}

impl Day for Day12 {
    type Input = Vec<Springs>;
    type Result = usize;

    fn day(&self) -> DayNum { 12 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.into_iter()
            .map(spring_ways)
            .sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        input.into_iter()
            .map(|(s, g)| {
                let mut new_s = s.clone();
                new_s.push('?');
                (new_s, g)
            })
            .map(|(s, g)| (s.repeat(5), g.repeat(5)))
            .map(|(s, g)| (s[..s.len() - 1].to_vec(), g))
            .map(spring_ways)
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day12::Day12;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "???.### 1,1,3
         .??..??...?##. 1,1,3
         ?#?#?#?#?#?#?#? 1,3,1,6
         ????.#...#... 4,1,1
         ????.######..#####. 1,6,5
         ?###???????? 3,2,1"};

    #[test]
    fn test12_1() {
        let day = Day12 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 21);
    }

    #[test]
    fn test12_2() {
        let day = Day12 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 525152);
    }
}
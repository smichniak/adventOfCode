use nom::bytes::complete::tag;
use nom::character::complete::{space1, u32 as nomU32};
use nom::sequence::{tuple, separated_pair, preceded};
use nom::multi::separated_list0;
use nom::error::ErrorKind;

use crate::day::{Day, DayNum};

type InputElement = (Vec<u32>, Vec<u32>);

fn parse_line(line: &str) -> InputElement {
    let start = tuple::<_, _, (_, ErrorKind), _>((tag("Card"), space1, nomU32, tag(":"), space1));
    let numbers1 = separated_list0(space1, nomU32);
    let numbers2 = separated_list0(space1, nomU32);
    let two_lists = separated_pair(numbers1, tuple((space1, tag("|"), space1)), numbers2);
    let (_, (nums1, nums2)) = preceded(start, two_lists)(line).unwrap();
    (nums1, nums2)
}

pub struct Day04 {}

impl Day for Day04 {
    type Input = Vec<InputElement>;
    type Result = usize;

    fn day(&self) -> DayNum { 04 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }


    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.iter()
            .map(|(winning, my)| my.iter().filter(|x| winning.contains(x)).count())
            .map(|size| match size {
                0 => 0,
                n => 1 << (n - 1)
            })
            .sum()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        let mut cards = vec![1usize; input.len()];
        input.iter()
            .map(|(winning, my)| my.iter().filter(|x| winning.contains(x)).count())
            .enumerate()
            .for_each(|(card, matches)|
                for card_offset in 0..matches {
                    cards[card + 1 + card_offset] += cards[card];
                });

        cards.iter().sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::day04::Day04;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
         Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
         Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
         Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
         Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
         Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"};

    #[test]
    fn test04_1() {
        let day = Day04 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 13);
    }

    #[test]
    fn test04_2() {
        let day = Day04 {};
        assert_eq!(day.solve2(day.parse_input(INPUT.to_string())), 30);
    }
}
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, u64 as nomU64};
use nom::sequence::separated_pair;
use nom::error::ErrorKind;

use std::cmp::max;
use itertools::Itertools;

use crate::day::{Day, DayNum};
use crate::utils::parse;

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Hand {
    hand_type: HandType,
    cards: Vec<CardType>,
    bid: u64,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum HandType {
    HighCard,
    Pair,
    TwoPair,
    Three,
    Full,
    Four,
    Five,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
enum CardType {
    Joker,
    Num(u64),
    T,
    J,
    Q,
    K,
    A,
}


fn get_card(c: char, joker: bool) -> CardType {
    match c {
        'A' => CardType::A,
        'K' => CardType::K,
        'Q' => CardType::Q,
        'J' => if joker { CardType::Joker } else { CardType::J },
        'T' => CardType::T,
        c => CardType::Num(c.to_digit(10).unwrap() as u64),
    }
}


fn get_type(cards: &Vec<CardType>) -> HandType {
    let counter = cards.iter().counts();
    let counts = counter.values();

    let hand_type = if counts.clone().contains(&5) {
        return HandType::Five;
    } else if counts.clone().contains(&4) {
        HandType::Four
    } else if counts.clone().contains(&3) && counts.clone().contains(&2) {
        HandType::Full
    } else if counts.clone().contains(&3) {
        HandType::Three
    } else if counts.clone().filter(|x| **x == 2).count() == 2 {
        HandType::TwoPair
    } else if counts.clone().contains(&2) {
        HandType::Pair
    } else {
        HandType::HighCard
    };

    match cards.iter().position(|x| *x == CardType::Joker) {
        None => hand_type,
        Some(position) => {
            let mut all_cards = vec![CardType::A, CardType::K, CardType::Q, CardType::T];
            all_cards.append(&mut (2..10).map(|x| CardType::Num(x)).collect_vec());
            let mut new_cards = cards.clone();
            all_cards.iter()
                .map(|c| get_type({
                    new_cards[position] = *c;
                    &new_cards
                })).fold(hand_type, max)
        }
    }
}

fn parse_line(line: &str, joker: bool) -> Hand {
    let line_parser = separated_pair(alphanumeric1::<_, (_, ErrorKind)>, tag(" "), nomU64);
    let (cards_str, bid) = parse(line_parser, line);
    let cards = cards_str.as_bytes().iter().map(|c| get_card(*c as char, joker)).collect_vec();
    Hand {
        hand_type: get_type(&cards),
        cards,
        bid,
    }
}

pub struct Day07 {}

impl Day for Day07 {
    type Input = Vec<Hand>;
    type Result = u64;

    fn day(&self) -> DayNum { 07 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| parse_line(l, false)).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        input.iter()
            .sorted()
            .enumerate()
            .map(|(i, hand)| (i as u64 + 1) * hand.bid)
            .sum()
    }

    fn parse_input2(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(|l| parse_line(l, true)).collect()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        self.solve1(input)
    }
}

#[cfg(test)]
mod tests {
    use std::cmp;
    use crate::day07::{CardType, Day07};
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "32T3K 765
         T55J5 684
         KK677 28
         KTJJT 220
         QQQJA 483"};

    #[test]
    fn test07_1() {
        let day = Day07 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 6440);
    }


    #[test]
    fn test07_1_card_ord() {
        let a = CardType::A;
        let k = CardType::K;
        // let q = CardType::Q;
        let t = CardType::T;
        let j = CardType::J;
        let nine = CardType::Num(9);
        let five = CardType::Num(5);
        let two = CardType::Num(2);
        let two2 = CardType::Num(2);

        assert_eq!(a.cmp(&k), cmp::Ordering::Greater);
        assert_eq!(a.cmp(&nine), cmp::Ordering::Greater);
        assert_eq!(a.cmp(&five), cmp::Ordering::Greater);

        assert_eq!(t.cmp(&j), cmp::Ordering::Less);

        assert_eq!(j.cmp(&nine), cmp::Ordering::Greater);
        assert_eq!(j.cmp(&five), cmp::Ordering::Greater);
        assert_eq!(j.cmp(&two), cmp::Ordering::Greater);

        assert_eq!(nine.cmp(&nine), cmp::Ordering::Equal);
        assert_eq!(nine.cmp(&five), cmp::Ordering::Greater);
        assert_eq!(nine.cmp(&two), cmp::Ordering::Greater);

        assert_eq!(five.cmp(&nine), cmp::Ordering::Less);
        assert_eq!(five.cmp(&five), cmp::Ordering::Equal);
        assert_eq!(five.cmp(&two), cmp::Ordering::Greater);

        assert_eq!(two.cmp(&nine), cmp::Ordering::Less);
        assert_eq!(two.cmp(&five), cmp::Ordering::Less);
        assert_eq!(two.cmp(&two), cmp::Ordering::Equal);

        assert_eq!(two.cmp(&two2), cmp::Ordering::Equal);
    }

    #[test]
    fn test07_2() {
        let day = Day07 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 5905);
    }
}
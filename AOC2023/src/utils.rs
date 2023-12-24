use itertools::Itertools;
use nom::IResult;
use nom::error::ParseError;
use nom::InputLength;

use crate::utils::Direction::{Up, Down, Left, Right};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub fn turn_right(i: isize, j: isize, dir: Direction) -> (isize, isize, Direction) {
    match dir {
        Up => (i, j + 1, Right),
        Down => (i, j - 1, Left),
        Left => (i - 1, j, Up),
        Right => (i + 1, j, Down),
    }
}

pub fn turn_left(i: isize, j: isize, dir: Direction) -> (isize, isize, Direction) {
    match dir {
        Up => (i, j - 1, Left),
        Down => (i, j + 1, Right),
        Left => (i + 1, j, Down),
        Right => (i - 1, j, Up),
    }
}

pub fn move_straight(i: isize, j: isize, dir: Direction) -> (isize, isize, Direction) {
    move_straight_distance(i, j, dir, 1)
}

pub fn move_straight_distance(i: isize, j: isize, dir: Direction, d: isize) -> (isize, isize, Direction) {
    match dir {
        Up => (i - d, j, dir),
        Down => (i + d, j, dir),
        Left => (i, j - d, dir),
        Right => (i, j + d, dir),
    }
}


pub fn parse<I, O, E, F>(mut parser: F, s: I) -> O
    where I: Clone + InputLength, F: FnMut(I) -> IResult<I, O, E>, E: ParseError<I> + std::fmt::Debug,
{
    let (_, r) = parser(s).unwrap();
    r
}

pub fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
    where
        T: Clone,
{
    assert!(!v.is_empty());
    (0..v[0].len())
        .map(|i| v.iter().map(|inner| inner[i].clone()).collect_vec())
        .collect()
}

pub fn rotate_left<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
    where
        T: Clone,
{
    assert!(!v.is_empty());
    transpose(v).into_iter().rev().collect_vec()
}

pub fn rotate_right<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
    where
        T: Clone,
{
    assert!(!v.is_empty());
    transpose(v.into_iter().rev().collect_vec())
}

pub fn is_palindrome<T>(v: &Vec<T>) -> bool
    where
        T: Eq + Clone,
{
    *v == v.iter().rev().cloned().collect_vec()
}
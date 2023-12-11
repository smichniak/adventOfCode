use itertools::Itertools;
use nom::IResult;
use nom::error::ParseError;
use nom::InputLength;


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
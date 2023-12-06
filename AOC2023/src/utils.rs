use nom::IResult;
use nom::error::ParseError;
use nom::InputLength;


pub fn parse<I, O, E, F>(mut parser: F, s: I) -> O
    where I: Clone + InputLength, F: FnMut(I) -> IResult<I, O, E>, E: ParseError<I> + std::fmt::Debug,
{
    let (_, r) = parser(s).unwrap();
    r
}
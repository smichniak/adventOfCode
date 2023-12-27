use nom::error::ErrorKind;
use nom::bytes::complete::tag;
use nom::character::complete::{i128 as nomI128, space1};
use nom::sequence::{pair, separated_pair, tuple};
use nom::multi::separated_list1;

use std::ops;

use crate::utils::parse;
use crate::day::{Day, DayNum};

#[derive(Copy, Clone, Debug)]
pub struct Point {
    x: i128,
    y: i128,
    z: i128,
}

impl ops::Neg for Point {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Point { x: -self.x, y: -self.y, z: -self.z }
    }
}

impl ops::Add for Point {
    type Output = Self;

    fn add(self, rhs: Point) -> Point {
        Point { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

impl ops::Sub for Point {
    type Output = Self;

    fn sub(self, rhs: Point) -> Point {
        self + (-rhs)
    }
}

impl ops::Mul<i128> for Point {
    type Output = Self;

    fn mul(self, rhs: i128) -> Self::Output {
        Point { x: rhs * self.x, y: rhs * self.y, z: rhs * self.z }
    }
}

fn parse_line(line: &str) -> (Point, Point) {
    let point_parser1 = separated_list1(pair(tag(","), space1), nomI128);
    let point_parser2 = separated_list1(pair(tag(","), space1), nomI128);
    let points = separated_pair::<_, _, _, _, (_, ErrorKind), _, _, _>
        (point_parser1, tuple((space1, tag("@"), space1)), point_parser2);

    let (p1, p2) = parse(points, line);

    (Point { x: p1[0], y: p1[1], z: p1[2] }, Point { x: p2[0], y: p2[1], z: p2[2] })
}

fn line_intersection_2d(
    Point { x: x1, y: y1, z: _ }: Point,
    Point { x: x2, y: y2, z: _ }: Point,
    Point { x: x3, y: y3, z: _ }: Point,
    Point { x: x4, y: y4, z: _ }: Point,
) -> Option<(i128, i128, i128, i128, i128)> {
    let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
    if denominator == 0 {
        return None;
    }

    let px = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4);
    let py = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4);
    let t = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4);
    let u = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2);

    Some((px, py, t, u, denominator))
}

fn get_future_intersections(points: Vec<(Point, Point)>) -> Vec<Point> {
    let mut intersections = Vec::new();
    for i in 0..points.len() {
        let (p1, dp1) = points[i];
        let p2 = p1 + dp1;
        for j in 0..i {
            let (p3, dp3) = points[j];
            let p4 = p3 + dp3;
            if let Some((px, py, t, u, denominator)) = line_intersection_2d(p1, p2, p3, p4) {
                if t / denominator >= 0 && u / denominator >= 0 {
                    intersections.push(Point { x: px / denominator, y: py / denominator, z: 0 });
                }
            }
        }
    }

    intersections
}

fn point_in_range(Point { x, y, z: _ }: Point, min_xy: i128, max_xy: i128) -> bool {
    min_xy <= x && x <= max_xy && min_xy <= y && y <= max_xy
}

pub struct Day24 {}

impl Day for Day24 {
    type Input = Vec<(Point, Point)>;
    type Result = usize;

    fn day(&self) -> DayNum { 24 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        get_future_intersections(input).into_iter()
            .filter(|&p| point_in_range(p, 200000000000000, 400000000000000))
            .count()
    }

    fn solve2(&self, input: Self::Input) -> Self::Result {
        // x0 + t0 x1 = I0 + t0 I1
        // x0 + t1 x1 = j0 + t1 j1
        // x0 + t2 x1 = k0 + t2 k1
        // x0 + t3 x1 = l0 + t3 l1

        // x0 = t0 (I1 - x1) + I0
        // t0 (I1 - x1) + I0 = j0 + t1 (j1 - x1)
        // t0 (I1 - x1) + I0 = k0 + t2 (k1 - x1)
        // t0 (I1 - x1) + I0 = l0 + t3 (l1 - x1)

        // j0 + t1 (j1 - x1) = k0 + t2 (k1 - x1)
        // j0 + t1 (j1 - x1) = l0 + t3 (l1 - x1)

        let (j0, j1) = input[0];
        let (k0, k1) = input[1];
        let (l0, l1) = input[2];

        let (Point { x: j0x, y: j0y, z: j0z }, Point { x: j1x, y: j1y, z: j1z }) = (j0, j1);
        let (Point { x: k0x, y: k0y, z: k0z }, Point { x: k1x, y: k1y, z: k1z }) = (k0, k1);
        let (Point { x: l0x, y: l0y, z: l0z }, Point { x: l1x, y: l1y, z: l1z }) = (l0, l1);

        let t3 = (-(j0x * j1z * k0y) + j0x * j1y * k0z - j0x * k0z * k1y + j0x * k0y * k1z -
            j0z * (j1y - k1y) * (k0x - l0x) + j0y * (j1z - k1z) * (k0x - l0x) + j1z * k0y * l0x -
            j1y * k0z * l0x + k0z * k1y * l0x - k0y * k1z * l0x + j0z * (j1x - k1x) * (k0y - l0y) +
            j0x * j1z * l0y - j1z * k0x * l0y + j1x * k0z * l0y - k0z * k1x * l0y - j0x * k1z * l0y +
            k0x * k1z * l0y - j0y * (j1x - k1x) * (k0z - l0z) + k0y * (-j1x + k1x) * l0z -
            (j0x - k0x) * (j1y - k1y) * l0z) / (j1z * k0y * k1x - j1y * k0z * k1x + j0x * j1z * k1y -
            j1z * k0x * k1y + j1x * k0z * k1y - j0x * j1y * k1z + j1y * k0x * k1z - j1x * k0y * k1z -
            j1z * k0y * l1x + j1y * k0z * l1x - k0z * k1y * l1x + k0y * k1z * l1x - j0x * j1z * l1y +
            j1z * k0x * l1y - j1x * k0z * l1y + k0z * k1x * l1y + j0x * k1z * l1y - k0x * k1z * l1y +
            j0z * (-(j1x * k1y) + j1y * (k1x - l1x) + k1y * l1x + j1x * l1y - k1x * l1y) +
            k0y * (j1x - k1x) * l1z + (j0x - k0x) * (j1y - k1y) * l1z +
            j0y * (-(j1z * k1x) + j1x * k1z + j1z * l1x - k1z * l1x - j1x * l1z + k1x * l1z));

        let t2 = (j0y * k0x - j0x * k0y - j0y * l0x + k0y * l0x + j0x * l0y - k0x * l0y +
            (j0y - k0y) * (j1x - l1x) * t3 - (j0x - k0x) * (j1y - l1y) * t3) /
            (-((j1y - k1y) * (j0x - l0x)) + (j1x - k1x) * (j0y - l0y) +
                (-(k1y * l1x) + j1y * (-k1x + l1x) + j1x * (k1y - l1y) + k1x * l1y) * t3);

        let t1 = (j0x * t2 - l0x * t2 - j0x * t3 + k0x * t3 + k1x * t2 * t3 - l1x * t2 * t3) /
            (k0x - l0x - j1x * t2 + k1x * t2 + j1x * t3 - l1x * t3);

        let x1x = (j0x - k0x + j1x * t1 - k1x * t2) / (t1 - t2);
        let x1y = (j0y - k0y + j1y * t1 - k1y * t2) / (t1 - t2);
        let x1z = (j0z - k0z + j1z * t1 - k1z * t2) / (t1 - t2);

        let x1 = Point { x: x1x, y: x1y, z: x1z };

        let x0 = j0 + (j1 - x1) * t1;

        (x0.x + x0.y + x0.z) as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::day24::{Day24, get_future_intersections, point_in_range};
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "19, 13, 30 @ -2,  1, -2
         18, 19, 22 @ -1, -1, -2
         20, 25, 34 @ -2, -2, -4
         12, 31, 28 @ -1, -2, -1
         20, 19, 15 @  1, -5, -3"};

    #[test]
    fn test24_1() {
        let day = Day24 {};

        assert_eq!(get_future_intersections(day.parse_input(INPUT.to_string())).into_iter()
                       .filter(|&p| point_in_range(p, 7, 27))
                       .count(), 2);
    }

    #[test]
    fn test24_2() {
        let day = Day24 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 47);
    }
}
defmodule Day06Test do
  use ExUnit.Case

  test "day 06 example 1" do
    input =
      """
      ....#.....
      .........#
      ..........
      ..#.......
      .......#..
      ..........
      .#..^.....
      ........#.
      #.........
      ......#...
      """

    assert input |> AOC.Day06.parser1() |> AOC.Day06.solution1() == 41
  end

  test "day 06 example 2" do
    input =
      """
      ....#.....
      .........#
      ..........
      ..#.......
      .......#..
      ..........
      .#..^.....
      ........#.
      #.........
      ......#...
      """

    assert input |> AOC.Day06.parser2() |> AOC.Day06.solution2() == 6
  end
end

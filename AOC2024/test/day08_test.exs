defmodule Day08Test do
  use ExUnit.Case

  test "day 08 example 1" do
    input =
      """
      ............
      ........0...
      .....0......
      .......0....
      ....0.......
      ......A.....
      ............
      ............
      ........A...
      .........A..
      ............
      ............
      """

    assert input |> AOC.Day08.parser1() |> AOC.Day08.solution1() == 14
  end

  test "day 08 example 2_0" do
    input =
      """
      T.........
      ...T......
      .T........
      ..........
      ..........
      ..........
      ..........
      ..........
      ..........
      ..........
      """

    assert input |> AOC.Day08.parser2() |> AOC.Day08.solution2() == 9
  end

  test "day 08 example 2" do
    input =
      """
      ............
      ........0...
      .....0......
      .......0....
      ....0.......
      ......A.....
      ............
      ............
      ........A...
      .........A..
      ............
      ............
      """

    assert input |> AOC.Day08.parser2() |> AOC.Day08.solution2() == 34
  end
end

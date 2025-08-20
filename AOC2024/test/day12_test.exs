defmodule Day12Test do
  use ExUnit.Case

  test "day 12 test 1" do
    input =
      """
      AAAA
      BBCD
      BBCC
      EEEC
      """

    assert input |> AOC.Day12.parser1() |> AOC.Day12.solution1() == 140
  end

  test "day 12 example 1" do
    input =
      """
      RRRRIICCFF
      RRRRIICCCF
      VVRRRCCFFF
      VVRCCCJFFF
      VVVVCJJCFE
      VVIVCCJJEE
      VVIIICJJEE
      MIIIIIJJEE
      MIIISIJEEE
      MMMISSJEEE
      """

    assert input |> AOC.Day12.parser1() |> AOC.Day12.solution1() == 1930
  end

  test "day 12 test 2_2" do
    input =
      """
      EEEEE
      EXXXX
      EEEEE
      EXXXX
      EEEEE
      """

    assert input |> AOC.Day12.parser2() |> AOC.Day12.solution2() == 236
  end

  test "day 12 example 2_2" do
    input =
      """
      AAAAAA
      AAABBA
      AAABBA
      ABBAAA
      ABBAAA
      AAAAAA
      """

    assert input |> AOC.Day12.parser2() |> AOC.Day12.solution2() == 368
  end

  test "day 12 example 2_3" do
    input =
      """
      AAAA
      BBCD
      BBCC
      EEEC
      """

    assert input |> AOC.Day12.parser2() |> AOC.Day12.solution2() == 80
  end

  test "day 12 test 2_4" do
    input =
      """
      A
      """

    assert input |> AOC.Day12.parser2() |> AOC.Day12.solution2() == 4
  end

  test "day 12 example 2" do
    input =
      """
      RRRRIICCFF
      RRRRIICCCF
      VVRRRCCFFF
      VVRCCCJFFF
      VVVVCJJCFE
      VVIVCCJJEE
      VVIIICJJEE
      MIIIIIJJEE
      MIIISIJEEE
      MMMISSJEEE
      """

    assert input |> AOC.Day12.parser2() |> AOC.Day12.solution2() == 1206
  end
end

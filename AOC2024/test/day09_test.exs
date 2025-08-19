defmodule Day09Test do
  use ExUnit.Case

  test "day 09 example 1_0" do
    input =
      """
      12345
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 60
  end

  test "day 09 example 1_1" do
    input =
      """
      11221
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 7
  end

  test "day 09 example 1_2" do
    input =
      """
      233313312141413140211
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 2132
  end

  test "day 09 example 1_3" do
    input =
      """
      233313312141413140256
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 3383
  end

  test "day 09 example 1_4" do
    input =
      """
      15101010101010101010105
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 825
  end

  test "day 09 example 1" do
    input =
      """
      2333133121414131402
      """

    assert input |> AOC.Day09.parser1() |> AOC.Day09.solution1() == 1928
  end

  test "day 09 example 2" do
    input =
      """
      2333133121414131402
      """

    assert input |> AOC.Day09.parser2() |> AOC.Day09.solution2() == 2858
  end
end

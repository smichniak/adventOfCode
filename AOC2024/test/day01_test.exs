defmodule Day01Test do
  use ExUnit.Case

  test "day 01 example 1" do
    input = """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

    assert input |> AOC.Day01.parser1() |> AOC.Day01.solution1() == 11
  end

  test "day 01 example 2" do
    input = """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

    assert input |> AOC.Day01.parser2() |> AOC.Day01.solution2() == 31
  end
end

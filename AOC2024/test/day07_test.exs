defmodule Day07Test do
  use ExUnit.Case

  test "day 07 example 1" do
    input =
      """
      190: 10 19
      3267: 81 40 27
      83: 17 5
      156: 15 6
      7290: 6 8 6 15
      161011: 16 10 13
      192: 17 8 14
      21037: 9 7 18 13
      292: 11 6 16 20
      """

    assert input |> AOC.Day07.parser1() |> AOC.Day07.solution1() == 3749
  end

  test "day 07 example 2" do
    input =
      """
      190: 10 19
      3267: 81 40 27
      83: 17 5
      156: 15 6
      7290: 6 8 6 15
      161011: 16 10 13
      192: 17 8 14
      21037: 9 7 18 13
      292: 11 6 16 20
      """

    assert input |> AOC.Day07.parser2() |> AOC.Day07.solution2() == 11387
  end
end

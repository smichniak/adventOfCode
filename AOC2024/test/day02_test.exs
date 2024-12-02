defmodule Day02Test do
  use ExUnit.Case

  test "day 02 example 1" do
    input =
      """
      7 6 4 2 1
      1 2 7 8 9
      9 7 6 2 1
      1 3 2 4 5
      8 6 4 4 1
      1 3 6 7 9
      """

    assert input |> AOC.Day02.parser1() |> AOC.Day02.solution1() == 2
  end

  test "day 02 example 2" do
    input =
      """
      7 6 4 2 1
      1 2 7 8 9
      9 7 6 2 1
      1 3 2 4 5
      8 6 4 4 1
      1 3 6 7 9
      """

    assert input |> AOC.Day02.parser2() |> AOC.Day02.solution2() == 4
  end
end

defmodule Day11Test do
  use ExUnit.Case

  test "day 11 example 1" do
    input =
      """
      125 17
      """

    assert input |> AOC.Day11.parser1() |> AOC.Day11.solution1() == 55312
  end
end

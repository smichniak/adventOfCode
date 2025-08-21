defmodule Day14Test do
  use ExUnit.Case

  @width 11
  @height 7
  @seconds 100

  test "day 14 example 1" do
    input =
      """
      p=0,4 v=3,-3
      p=6,3 v=-1,-3
      p=10,3 v=-1,2
      p=2,0 v=2,-1
      p=0,0 v=1,3
      p=3,0 v=-2,-2
      p=7,6 v=-1,-3
      p=3,0 v=-1,-2
      p=9,3 v=2,3
      p=7,3 v=-1,2
      p=2,4 v=2,-3
      p=9,5 v=-3,-3
      """

    assert input |> AOC.Day14.parser1() |> AOC.Day14.solution1(@width, @height, @seconds) == 12
  end

  test "day 14 example 2" do
    input =
      """
      """

    assert input |> AOC.Day14.parser2() |> AOC.Day14.solution2() == :TODO
  end
end

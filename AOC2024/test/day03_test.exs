defmodule Day03Test do
  use ExUnit.Case

  test "day 03 example 1" do
    input =
      """
      xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
      """

    assert input |> AOC.Day03.parser1() |> AOC.Day03.solution1() == 161
  end

  test "day 03 example 2" do
    input =
      """
      xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
      """

    assert input |> AOC.Day03.parser2() |> AOC.Day03.solution2() == 48
  end
end

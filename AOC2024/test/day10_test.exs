defmodule Day10Test do
  use ExUnit.Case

  test "day 10 test 1" do
    input =
      """
      8880888
      8881888
      8882888
      6543456
      7888887
      8888888
      9888889
      """

    assert input |> AOC.Day10.parser1() |> AOC.Day10.solution1() == 2
  end

  test "day 10 test 2" do
    input =
      """
      5590559
      5551598
      5552557
      6543456
      7655987
      8765555
      9875555
      """

    assert input |> AOC.Day10.parser1() |> AOC.Day10.solution1() == 4
  end

  test "day 10 test 3" do
    input =
      """
      1055955
      2555855
      3555755
      4567654
      5558553
      5559552
      5555501
      """

    assert input |> AOC.Day10.parser1() |> AOC.Day10.solution1() == 3
  end

  test "day 10 example 1" do
    input =
      """
      89010123
      78121874
      87430965
      96549874
      45678903
      32019012
      01329801
      10456732
      """

    assert input |> AOC.Day10.parser1() |> AOC.Day10.solution1() == 36
  end

  test "day 10 example 2" do
    input =
      """
      89010123
      78121874
      87430965
      96549874
      45678903
      32019012
      01329801
      10456732
      """

    assert input |> AOC.Day10.parser2() |> AOC.Day10.solution2() == 81
  end
end

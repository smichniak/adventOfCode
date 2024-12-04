defmodule Day04Test do
  use ExUnit.Case

  test "day 04 example 1" do
    input =
      """
      MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
      """

    assert input |> AOC.Day04.parser1() |> AOC.Day04.solution1() == 18
  end

  test "day 04 example 2" do
    input =
      """
      MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
      """

    assert input |> AOC.Day04.parser2() |> AOC.Day04.solution2() == 9
  end
end

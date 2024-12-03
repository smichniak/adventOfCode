defmodule AOC.Day03 do
  @type input_type :: [[integer()]]

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    mul_regex = ~r"mul\((\d+),(\d+)\)"

    text_input
    |> then(&Regex.scan(mul_regex, &1))
    |> Enum.map(fn [_, x, y] -> [String.to_integer(x), String.to_integer(y)] end)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input) do
    op_regex = ~r"(?:mul\((\d+),(\d+)\))|(?:do\(\))|(?:don't\(\))"

    text_input
    |> then(&Regex.scan(op_regex, &1))
    |> Enum.map(fn x ->
      case x do
        [_, x, y] -> {String.to_integer(x), String.to_integer(y)}
        ["do()"] -> :do
        ["don't()"] -> :dont
      end
    end)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    input
    |> Enum.map(fn nums -> Enum.reduce(nums, &*/2) end)
    |> Enum.sum()
  end

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    input
    |> Enum.reduce({0, :do}, fn a, {num, mul} ->
      case {a, mul} do
        {{x, y}, :do} -> {num + x * y, :do}
        {{_, _}, :dont} -> {num, :dont}
        {new_mul, _} -> {num, new_mul}
      end
    end)
    |> elem(0)
  end
end

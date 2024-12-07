defmodule AOC.Day07 do
  @type int_and_list :: {integer(), [integer()]}
  @type input_type :: [int_and_list()]

  @spec parse_line(binary()) :: {integer(), [integer()]}
  def parse_line(line) do
    [test_value, nums] = String.split(line, ": ")
    {String.to_integer(test_value), String.split(nums, " ") |> Enum.map(&String.to_integer/1)}
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec can_be_true?(int_and_list(), [(integer(), integer() -> integer())], integer()) :: boolean()
  def can_be_true?({x, _ops}, _nums, acc) when acc > x, do: false
  def can_be_true?({x, []}, _ops, x), do: true
  def can_be_true?({x, []}, _ops, acc) when x != acc, do: false

  def can_be_true?({test_value, [x | t]}, ops, acc) do
    Enum.any?(ops, fn op -> can_be_true?({test_value, t}, ops, op.(acc, x)) end)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    input
    |> Enum.filter(fn test -> can_be_true?(test, [&+/2, &*/2], 0) end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  @spec concat(integer(), integer()) :: integer()
  def concat(a, b), do: String.to_integer("#{a}#{b}")

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    input
    |> Enum.filter(fn test -> can_be_true?(test, [&+/2, &*/2, &concat/2], 0) end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end
end

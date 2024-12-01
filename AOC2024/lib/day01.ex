defmodule AOC.Day01 do
  @moduledoc """
  Advent of Code 2023, day 1.
  """

  use Utils

  @type int_list_pair :: Utils.int_list_pair()
  @type input_type :: int_list_pair()

  @spec parse_line(binary()) :: {integer(), integer()}
  def parse_line(line) do
    line
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.unzip()
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    input
    |> Tuple.to_list()
    |> Enum.map(&Enum.sort/1)
    |> Enum.zip()
    |> Enum.map(fn {x, y} -> abs(x - y) end)
    |> Enum.sum()
  end

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    {list1, list2} = input
    counter = Utils.counter(list2)

    list1
    |> Enum.map(fn x -> x * Map.get(counter, x, 0) end)
    |> Enum.sum()
  end
end

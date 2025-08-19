defmodule AOC.Day11 do
  use Utils

  @type int_map :: Utils.int_map()
  @type input_type :: [integer()]

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.trim()
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec stone_rules({integer(), integer()}) :: [{integer(), integer()}]
  def stone_rules({0, count}), do: [{1, count}]

  def stone_rules({stone, count}) do
    stone_str = to_string(stone)
    len = String.length(stone_str)

    if rem(len, 2) == 0 do
      {left, right} = String.split_at(stone_str, div(len, 2))
      [{String.to_integer(left), count}, {String.to_integer(right), count}]
    else
      [{stone * 2024, count}]
    end
  end

  @spec blink(int_map()) :: int_map()
  def blink(stones) do
    stones
    |> Enum.flat_map(&stone_rules/1)
    |> Enum.sort_by(fn {stone, _} -> stone end)
    |> Enum.chunk_by(fn {stone, _} -> stone end)
    |> Enum.map(fn stones -> {elem(Enum.at(stones, 0), 0), stones |> Enum.map(&elem(&1, 1)) |> Enum.sum()} end)
    |> Enum.into(%{})
  end

  @spec stone_count(input_type(), integer()) :: integer()
  def stone_count(stones, blinks) do
    stones
    |> Utils.counter()
    |> Stream.iterate(&blink/1)
    |> Enum.at(blinks)
    |> Enum.map(fn {_stone, count} -> count end)
    |> Enum.sum()
  end

  @spec solution1(input_type()) :: integer()
  def solution1(stones) do
    stone_count(stones, 25)
  end

  @spec solution2(input_type()) :: integer()
  def solution2(stones) do
    stone_count(stones, 75)
  end
end

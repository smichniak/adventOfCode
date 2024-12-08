defmodule AOC.Day08 do
  use Utils

  @type coordinate :: Utils.coordinate()
  @type input_type :: {%{String.grapheme() => [coordinate()]}, coordinate()}

  def parse_line(line, y, map) do
    line
    |> String.split("", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(map, fn {char, x}, acc -> Map.update(acc, char, [{y, x}], &[{y, x} | &1]) end)
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    rows = text_input |> String.split("\n", trim: true)

    map =
      rows
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc -> parse_line(line, y, acc) end)
      |> Map.delete(".")

    {map, {Enum.count(rows) - 1, String.length(Enum.at(rows, 0)) - 1}}
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec handle_frequency([coordinate()]) :: [coordinate()]
  def handle_frequency(locations) do
    for {y1, x1} = c1 <- locations, {y2, x2} = c2 <- locations, c1 != c2 do
      {2 * y2 - y1, 2 * x2 - x1}
    end
  end

  @spec in_bounds?(coordinate(), coordinate()) :: boolean()
  def in_bounds?({y, x}, {max_y, max_x}), do: y >= 0 and y <= max_y and x >= 0 and x <= max_x

  @spec solution1(input_type()) :: integer()
  def solution1({map, bounds}) do
    map
    |> Map.values()
    |> Enum.map(&handle_frequency/1)
    |> Enum.concat()
    |> Enum.uniq()
    |> Enum.count(&in_bounds?(&1, bounds))
  end

  @spec handle_frequency_resonant([coordinate()], coordinate()) :: [coordinate()]
  def handle_frequency_resonant(locations, {max_y, max_x}) do
    for {y1, x1} = c1 <- locations, {y2, x2} = c2 <- locations, c1 != c2 do
      gcd = Integer.gcd(y2 - y1, x2 - x1)

      for i <- 0..max(max_y, max_x) do
        {y1 + i * (y2 - y1) / gcd, x1 + i * (x2 - x1) / gcd}
      end
    end
    |> Enum.concat()
  end

  @spec solution2(input_type()) :: integer()
  def solution2({map, bounds}) do
    map
    |> Map.values()
    |> Enum.map(&handle_frequency_resonant(&1, bounds))
    |> Enum.concat()
    |> Enum.uniq()
    |> Enum.count(&in_bounds?(&1, bounds))
  end
end

defmodule AOC.Day12 do
  use Utils

  @type coordinate :: Utils.coordinate()
  @type coordinate_set :: MapSet.t(coordinate())
  @type input_type :: %{coordinate() => String.grapheme()}

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, y} ->
      row
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {char, x} -> {{y, x}, char} end)
    end)
    |> Enum.into(%{})
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec regions(input_type()) :: [coordinate_set()]
  defp regions(garden) do
    garden
    |> Map.keys()
    |> Enum.reduce({MapSet.new(), []}, fn coord, {visited, regions} ->
      if coord in visited do
        {visited, regions}
      else
        region = explore_region(garden, coord, Map.get(garden, coord))
        {MapSet.union(visited, region), [region | regions]}
      end
    end)
    |> elem(1)
  end

  @spec explore_region(input_type(), coordinate(), String.grapheme()) :: coordinate_set()
  defp explore_region(garden, start_coord, char) do
    do_explore_region(garden, [start_coord], char, MapSet.new())
  end

  @spec do_explore_region(input_type(), [coordinate()], String.grapheme(), coordinate_set()) :: coordinate_set()
  defp do_explore_region(_garden, [], _char, region), do: region

  defp do_explore_region(garden, [coord | rest], char, region) do
    if coord in region or Map.get(garden, coord) != char do
      do_explore_region(garden, rest, char, region)
    else
      {y, x} = coord
      new_region = MapSet.put(region, coord)

      neighbors =
        direction_change()
        |> Enum.map(fn {dy, dx} -> {y + dy, x + dx} end)
        |> Enum.filter(&(Map.get(garden, &1) == char and &1 not in new_region))

      do_explore_region(garden, neighbors ++ rest, char, new_region)
    end
  end

  @spec region_area(coordinate_set()) :: integer()
  defp region_area(region) do
    MapSet.size(region)
  end

  @spec region_perimeter(coordinate_set()) :: integer()
  defp region_perimeter(region) do
    region
    |> MapSet.to_list()
    |> Enum.flat_map(fn {y, x} ->
      direction_change()
      |> Enum.map(fn {dy, dx} -> {y + dy, x + dx} end)
    end)
    |> Enum.count(fn coord -> !MapSet.member?(region, coord) end)
  end

  @spec region_price(coordinate_set()) :: integer()
  defp region_price(region) do
    region_area(region) * region_perimeter(region)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    input
    |> regions()
    |> Enum.map(&region_price/1)
    |> Enum.sum()
  end

  @spec top_border_fields(coordinate_set()) :: [coordinate()]
  defp top_border_fields(region) do
    region
    |> Enum.to_list()
    |> Enum.filter(fn {y, x} -> !MapSet.member?(region, {y - 1, x}) end)
  end

  @spec bottom_border_fields(coordinate_set()) :: [coordinate()]
  defp bottom_border_fields(region) do
    region
    |> Enum.to_list()
    |> Enum.filter(fn {y, x} -> !MapSet.member?(region, {y + 1, x}) end)
  end

  @spec row_sides([coordinate()]) :: integer()
  defp row_sides(row) do
    row
    |> Enum.map(&elem(&1, 1))
    |> Enum.sort()
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.count(fn [a, b] -> b - a > 1 end)
    |> then(&(&1 + 1))
  end

  @spec region_row_sides([coordinate()]) :: integer()
  defp region_row_sides(region_coordinates) do
    region_coordinates
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.chunk_by(&elem(&1, 0))
    |> Enum.map(&row_sides/1)
    |> Enum.sum()
  end

  @spec region_sides(coordinate_set()) :: integer()
  defp region_sides(region) do
    top_row = region |> top_border_fields()
    bottom_row = region |> bottom_border_fields()

    region_coordinates_flipped = region |> MapSet.to_list() |> Enum.map(fn {y, x} -> {x, y} end) |> MapSet.new()
    left_column = region_coordinates_flipped |> top_border_fields()
    right_column = region_coordinates_flipped |> bottom_border_fields()

    region_row_sides(top_row) +
      region_row_sides(bottom_row) +
      region_row_sides(left_column) +
      region_row_sides(right_column)
  end

  @spec region_sides_price(coordinate_set()) :: integer()
  defp region_sides_price(region) do
    region_area(region) * region_sides(region)
  end

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    input
    |> regions()
    |> Enum.map(&region_sides_price/1)
    |> Enum.sum()
  end
end

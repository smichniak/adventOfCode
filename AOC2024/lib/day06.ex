defmodule AOC.Day06 do
  use Utils

  @type coordinate :: Utils.coordinate()
  @type coordinate_map :: %{coordinate => String.grapheme()}
  @type input_type :: {coordinate_map(), coordinate()}

  defp direction_change(), do: [{-1, 0}, {0, 1}, {1, 0}, {0, -1}]

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    map =
      text_input
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)
      |> Enum.map(&Enum.with_index/1)
      |> Enum.with_index()
      |> Enum.map(fn {row, y} -> Enum.map(row, fn {elem, x} -> {{y, x}, elem} end) end)
      |> List.flatten()
      |> Enum.into(%{})

    {{start_y, start_x}, _} = map |> Enum.find(fn {{_, _}, elem} -> elem == "^" end)

    {map, {start_y, start_x}}
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec take_step(coordinate_map(), coordinate(), integer()) :: {coordinate(), integer()} | atom()
  def take_step(map, {y, x} = coordinate, direction) do
    {dy, dx} = Enum.at(direction_change(), direction)
    new_coordinate = {y + dy, x + dx}

    case Map.get(map, new_coordinate) do
      c when c in ["^", "."] -> {new_coordinate, direction}
      "#" -> {coordinate, rem(direction + 1, 4)}
      nil -> :end
    end
  end

  @spec walk1(coordinate_map(), coordinate(), integer(), MapSet.t(integer())) :: MapSet.t(integer()) | atom()
  def walk1(map, coordinate, direction, visited) do
    visited = MapSet.put(visited, {coordinate, direction})

    case take_step(map, coordinate, direction) do
      {new_coordinate, new_direction} -> walk1(map, new_coordinate, new_direction, visited)
      :end -> visited
    end
  end

  @spec solution1(input_type()) :: integer()
  def solution1({map, {start_y, start_x}}) do
    walk1(map, {start_y, start_x}, 0, MapSet.new())
    |> MapSet.to_list()
    |> Enum.map(&elem(&1, 0))
    |> Enum.uniq()
    |> Enum.count()
  end

  @spec walk2(coordinate_map(), coordinate(), integer(), integer(), coordinate()) :: atom()
  def walk2(_, _, _, steps, {max_y, max_x}) when steps >= (max_x + 1) * (max_y + 1), do: :loop

  def walk2(map, coordinate, direction, steps, max_coords) do
    case take_step(map, coordinate, direction) do
      {new_coordinate, new_direction} -> walk2(map, new_coordinate, new_direction, steps + 1, max_coords)
      :end -> :end
    end
  end

  @spec solution2(input_type()) :: integer()
  def solution2({map, start_coord}) do
    max_coords =
      map
      |> Map.keys()
      |> Enum.reduce({0, 0}, fn {y, x}, {max_y, max_x} -> {max(max_y, y), max(max_x, x)} end)

    map
    |> Map.keys()
    |> Enum.filter(&(&1 != start_coord))
    |> Utils.pmap(&walk2(Map.put(map, &1, "#"), start_coord, 0, 0, max_coords))
    |> Enum.count(&(&1 == :loop))
  end
end

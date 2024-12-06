defmodule AOC.Day06 do
  use Utils

  @type coordinate :: Utils.coordinate()
  @type coordinate_map :: %{coordinate => String.grapheme()}
  @type input_type :: {coordinate_map(), coordinate()}

  defp directions(), do: [:up, :right, :down, :left]
  defp direction_change(), do: %{:up => {-1, 0}, :right => {0, 1}, :down => {1, 0}, :left => {0, -1}}

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

  @spec turn_right(atom()) :: atom()
  def turn_right(direction) do
    directions()
    |> Enum.find_index(&(&1 == direction))
    |> Kernel.+(1)
    |> Kernel.rem(4)
    |> then(&Enum.at(directions(), &1))
  end

  @spec take_step(coordinate_map(), coordinate(), atom(), MapSet.t(integer())) :: MapSet.t(integer()) | atom()
  def take_step(map, {y, x} = coordinate, direction, visited) do
    if MapSet.member?(visited, {coordinate, direction}) do
      :loop
    else
      visited = MapSet.put(visited, {coordinate, direction})

      {dy, dx} = direction_change()[direction]
      new_coordinate = {y + dy, x + dx}

      case Map.get(map, new_coordinate) do
        c when c in ["^", "."] -> take_step(map, new_coordinate, direction, visited)
        "#" -> take_step(map, coordinate, turn_right(direction), visited)
        nil -> visited
      end
    end
  end

  @spec solution1(input_type()) :: integer()
  def solution1({map, {start_y, start_x}}) do
    take_step(map, {start_y, start_x}, :up, MapSet.new())
    |> MapSet.to_list()
    |> Enum.map(&elem(&1, 0))
    |> Enum.uniq()
    |> Enum.count()
  end

  @spec solution2(input_type()) :: integer()
  def solution2({map, start_coord}) do
    map
    |> Map.keys()
    |> Enum.filter(&(&1 != start_coord))
    |> Utils.pmap(&take_step(Map.put(map, &1, "#"), start_coord, :up, MapSet.new()))
    |> Enum.count(&(&1 == :loop))
  end
end

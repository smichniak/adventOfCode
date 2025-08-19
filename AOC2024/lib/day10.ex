defmodule AOC.Day10 do
  use Utils

  @type coordinate :: Utils.coordinate()
  @type input_type :: %{coordinate() => integer()}

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {row, y} ->
      row
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {height, x} -> {{y, x}, String.to_integer(height)} end)
    end)
    |> List.flatten()
    |> Enum.into(%{})
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec ascend([coordinate()], input_type()) :: [coordinate()]
  def ascend(coordinates, height_map) do
    for {y, x} = coordinate <- coordinates,
        {dy, dx} <- direction_change(),
        new_coordinate = {y + dy, x + dx},
        Map.get(height_map, new_coordinate) == Map.get(height_map, coordinate) + 1 do
      new_coordinate
    end
  end

  @spec get_score([coordinate()], input_type(), MapSet.t(coordinate())) :: integer()
  def get_score(coordinates, height_map, score \\ MapSet.new())
  def get_score([], _height_map, score), do: MapSet.size(score)

  def get_score(coordinates, height_map, score) do
    new_coordinates = ascend(coordinates, height_map)
    {peaks, non_peaks} = Enum.split_with(new_coordinates, &(Map.get(height_map, &1) == 9))
    get_score(non_peaks, height_map, MapSet.union(score, MapSet.new(peaks)))
  end

  @spec trailheads(input_type()) :: [coordinate()]
  def trailheads(height_map) do
    height_map
    |> Enum.filter(&match?({_, 0}, &1))
    |> Enum.map(fn {coordinate, _} -> coordinate end)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(height_map) do
    height_map
    |> trailheads()
    |> Enum.map(&get_score([&1], height_map))
    |> Enum.sum()
  end

  @spec get_rating([coordinate()], input_type(), integer()) :: integer()
  def get_rating(coordinates, height_map, rating \\ 0)
  def get_rating([], _height_map, rating), do: rating

  def get_rating(coordinates, height_map, rating) do
    new_coordinates = ascend(coordinates, height_map)
    {peaks, non_peaks} = Enum.split_with(new_coordinates, &(Map.get(height_map, &1) == 9))
    get_rating(non_peaks, height_map, rating + Enum.count(peaks))
  end

  @spec solution2(input_type()) :: integer()
  def solution2(height_map) do
    height_map
    |> trailheads()
    |> get_rating(height_map)
  end
end

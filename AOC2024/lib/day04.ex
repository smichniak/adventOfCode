defmodule AOC.Day04 do
  use Utils

  @type input_type :: [String.t()]

  @spec get_diagonals(input_type()) :: input_type()
  def get_diagonals(matrix) do
    row_length = Enum.at(matrix, 0) |> Enum.count()

    matrix
    |> Enum.with_index()
    |> Enum.map(fn {row, row_index} -> Utils.shift_left(row, row_index) end)
    |> Utils.transpose()
    |> Enum.with_index()
    |> Enum.map(fn {row, row_index} -> Enum.take(row, row_length - row_index) end)
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    lines = String.split(text_input, "\n", trim: true) |> Enum.map(&String.graphemes/1)
    columns = Utils.transpose(lines)
    lines_rev = Enum.map(lines, &Enum.reverse/1)
    cols_rev = Utils.transpose(lines_rev)

    top_row_diagonals = get_diagonals(lines)
    bottom_row_diagonals = get_diagonals(lines_rev)
    # tl to remove the repeating main diagonal
    left_column_diagonals_down = get_diagonals(columns) |> tl()
    left_column_diagonals_up = get_diagonals(cols_rev) |> tl()

    one_direction =
      lines ++
        columns ++
        top_row_diagonals ++
        bottom_row_diagonals ++
        left_column_diagonals_down ++
        left_column_diagonals_up

    another_direction = Enum.map(one_direction, &Enum.reverse/1)

    (one_direction ++ another_direction) |> Enum.map(&Enum.join/1)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    pattern = ~r"XMAS"

    input
    |> Enum.map(fn str -> Regex.scan(pattern, str) |> Enum.count() end)
    |> Enum.sum()
  end

  @spec mas_match?([[String.grapheme()]]) :: boolean()
  def mas_match?(block) do
    case block do
      [["M", _, "M"], [_, "A", _], ["S", _, "S"]] -> true
      [["M", _, "S"], [_, "A", _], ["M", _, "S"]] -> true
      [["S", _, "M"], [_, "A", _], ["S", _, "M"]] -> true
      [["S", _, "S"], [_, "A", _], ["M", _, "M"]] -> true
      _ -> false
    end
  end

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    input
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.map(&Utils.transpose/1)
    |> Enum.flat_map(fn col -> Enum.chunk_every(col, 3, 1, :discard) end)
    |> Enum.count(&mas_match?/1)
  end
end

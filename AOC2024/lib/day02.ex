defmodule AOC.Day02 do
  @type input_type :: [[integer()]]

  @spec parse_line(binary()) :: [integer()]
  def parse_line(line) do
    line
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec ordered?([integer()]) :: boolean()
  def ordered?(report) do
    sorted = Enum.sort(report)
    report == sorted or report == Enum.reverse(sorted)
  end

  @spec consecutive_diffs([integer()]) :: [integer()]
  def consecutive_diffs(report) do
    Enum.zip_with(report, Enum.drop(report, 1), fn x, y -> abs(x - y) end)
  end

  @spec safe_report?([integer()]) :: boolean()
  def safe_report?(report) do
    {min_diff, max_diff} = consecutive_diffs(report) |> Enum.min_max()

    min_diff >= 1 and max_diff <= 3 and ordered?(report)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(input) do
    input
    |> Enum.filter(&safe_report?/1)
    |> Enum.count()
  end

  @spec safe_report_dampner?([integer()]) :: boolean()
  def safe_report_dampner?(report) do
    0..Enum.count(report)
    |> Enum.map(fn index -> Enum.split(report, index) end)
    |> Enum.map(fn {left, right} -> left ++ Enum.drop(right, 1) end)
    |> Enum.map(&safe_report?/1)
    |> Enum.any?()
  end

  @spec solution2(input_type()) :: integer()
  def solution2(input) do
    input
    |> Enum.filter(&safe_report_dampner?/1)
    |> Enum.count()
  end
end

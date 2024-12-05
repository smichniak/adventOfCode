defmodule AOC.Day05 do
  use Utils

  @type set_map :: %{integer() => MapSet.t(integer())}
  @type input_type :: {set_map(), [[integer()]]}

  @spec prase_rules([String.t()]) :: set_map()
  def prase_rules(rules) do
    rules
    |> Enum.map(&String.split(&1, "|", trim: true))
    |> Enum.map(fn num_strs -> Enum.map(num_strs, &String.to_integer/1) end)
    |> Enum.map(fn [a, b] -> {a, b} end)
    |> Enum.reduce(%{}, fn {b, a}, acc -> Map.update(acc, a, MapSet.new([b]), &MapSet.put(&1, b)) end)
  end

  @spec parse_pages([String.t()]) :: [list(integer())]
  def parse_pages(pages) do
    pages
    |> Enum.map(&String.split(&1, ",", trim: true))
    |> Enum.map(fn num_strs -> Enum.map(num_strs, &String.to_integer/1) end)
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.split("\n", trim: true)
    |> Enum.split_with(&String.contains?(&1, "|"))
    |> then(fn {rules, pages} -> {prase_rules(rules), parse_pages(pages)} end)
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec valid_update?([integer()], set_map()) :: boolean()
  def valid_update?(page, rules) do
    page
    |> Enum.with_index()
    |> Enum.map(fn {_num, idx} -> Enum.drop(page, idx) end)
    |> Enum.reduce_while(false, fn [a | t], _acc ->
      if Enum.any?(t, fn x -> Map.get(rules, a, MapSet.new()) |> MapSet.member?(x) end) do
        {:halt, true}
      else
        {:cont, false}
      end
    end)
    |> then(&(not &1))
  end

  @spec solution1(input_type()) :: integer()
  def solution1({rules, pages}) do
    pages
    |> Enum.filter(&valid_update?(&1, rules))
    |> Enum.map(&Utils.middle_list_element/1)
    |> Enum.sum()
  end

  @spec valid_update?([integer()], set_map()) :: [integer()]
  def correct_order(page, rules) do
    page
    |> Enum.sort(fn a, b -> Map.get(rules, b, MapSet.new()) |> MapSet.member?(a) end)
  end

  @spec solution2(input_type()) :: integer()
  def solution2({rules, pages}) do
    pages
    |> Enum.reject(&valid_update?(&1, rules))
    |> Enum.map(&correct_order(&1, rules))
    |> Enum.map(&Utils.middle_list_element/1)
    |> Enum.sum()
  end
end

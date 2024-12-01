defmodule AOC do
  @moduledoc """
  Advent of Code helper functions and macros
  """

  defmacro __using__(_) do
    quote do
      import AOC
    end
  end

  @type int_list_pair :: {list(integer()), list(integer())}

  @solution_range 1..25

  def solution_range, do: @solution_range

  def pad_day(day), do: String.pad_leading(day, 2, "0")

  def read_text(path) do
    with {:ok, file} <- File.read(path), do: file |> String.trim_trailing()
  end

  @spec get_input(binary()) :: binary() | {:error, atom()}
  def get_input(day), do: read_text("input/day#{pad_day(day)}.in")

  def get_solution_module(day), do: "Elixir.AOC.Day#{pad_day(day)}" |> String.to_existing_atom()

  def get_parser(part), do: "parser#{part}" |> String.to_atom()

  def get_solver(part), do: "solution#{part}" |> String.to_atom()

  def solve_part(day, part, input) do
    try do
      day_module = get_solution_module(day)

      parser = get_parser(part)
      solver = get_solver(part)

      parsed_input = apply(day_module, parser, [input])
      solution = apply(day_module, solver, [parsed_input])

      {:ok, solution}
    rescue
      ArgumentError -> {:error, "Day module not implemented"}
      UndefinedFunctionError -> {:error, "Day function not implemented"}
    end
  end

  def solve_day(day, input) do
    {solve_part(day, 1, input), solve_part(day, 2, input)}
  end
end

defmodule Mix.Tasks.Solve do
  use Mix.Task

  use AOC

  defp print_day_solution(day, part, solution) do
    "Day #{AOC.pad_day(day)}, part #{part}: #{solution}" |> IO.puts()
  end

  defp handle_part_result(day, part, result) do
    case result do
      {:ok, solution} -> print_day_solution(day, part, solution)
      {:error, reason} -> print_day_solution(day, part, reason)
    end
  end

  @shortdoc "Solve AOC"
  def run([day, part]) do
    input = AOC.get_input(day)
    result = AOC.solve_part(day, part, input)
    handle_part_result(day, part, result)
  end

  def run(["today"]) do
    today = Date.utc_today().day |> Integer.to_string()
    run([today])
  end

  def run([day]) do
    input = AOC.get_input(day)
    {result1, result2} = AOC.solve_day(day, input)

    handle_part_result(day, 1, result1)
    handle_part_result(day, 2, result2)
  end

  def run([]) do
    Enum.each(AOC.solution_range(), fn day -> run([Integer.to_string(day)]) end)
  end
end

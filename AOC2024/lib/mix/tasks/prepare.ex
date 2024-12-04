defmodule Mix.Tasks.Prepare do
  use Mix.Task

  use AOC

  @shortdoc "Solve AOC"
  def run([day]) do
    padded_day = AOC.pad_day(day)

    day_input_path = "input/day#{padded_day}.in"
    day_module_path = "lib/day#{padded_day}.ex"
    day_test_path = "test/day#{padded_day}_test.exs"

    unless File.exists?(day_input_path) do
      File.touch!(day_input_path)
      IO.puts("Created Input for day #{day}")
    else
      IO.puts("Input for day #{day} already exist")
    end

    unless File.exists?(day_module_path) do
      File.write!(day_module_path, day_module_template(padded_day))
      IO.puts("Created Module for day #{day}")
    else
      IO.puts("Module for day #{day} already exist")
    end

    unless File.exists?(day_test_path) do
      File.write!(day_test_path, day_test_template(padded_day))
      IO.puts("Created Test for day #{day}")
    else
      IO.puts("Test for day #{day} already exist")
    end
  end

  def run([]) do
    Date.utc_today().day |> Integer.to_string() |> (&run([&1])).()
  end

  defp day_module_template(day) do
    """
    defmodule AOC.Day#{day} do
      @type input_type :: :TODO

      @spec parser1(binary()) :: input_type()
      def parser1(_text_input) do
        :TODO
      end

      @spec parser2(binary()) :: input_type()
      def parser2(text_input), do: parser1(text_input)

      @spec solution1(input_type()) :: integer()
      def solution1(_input) do
        :TODO
      end

      @spec solution2(input_type()) :: integer()
      def solution2(_input) do
        :TODO
      end
    end
    """
  end

  defp day_test_template(day) do
    """
    defmodule Day#{day}Test do
      use ExUnit.Case

      test "day #{day} example 1" do
        input =
          \"\"\"
          \"\"\"

        assert input |> AOC.Day#{day}.parser1() |> AOC.Day#{day}.solution1() == :TODO
      end

      test "day #{day} example 2" do
        input =
          \"\"\"
          \"\"\"

        assert input |> AOC.Day#{day}.parser2() |> AOC.Day#{day}.solution2() == :TODO
      end
    end
    """
  end
end

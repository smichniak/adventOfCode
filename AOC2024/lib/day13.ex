defmodule AOC.Day13 do
  defmodule Machine do
    defstruct [:ax, :ay, :bx, :by, :px, :py]

    @type t :: %__MODULE__{
            ax: integer(),
            ay: integer(),
            bx: integer(),
            by: integer(),
            px: integer(),
            py: integer()
          }
  end

  @type input_type :: [Machine.t()]

  @a_price 3
  @b_price 1
  @max_uses 100
  @prize_error 10_000_000_000_000

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.trim()
    |> String.split("\n\n")
    |> Enum.map(&parse_machine/1)
  end

  defp parse_machine(machine_text) do
    lines = String.split(machine_text, "\n")

    # Parse Button A: X+94, Y+34
    [ax, ay] =
      Regex.run(~r/Button A: X\+(\d+), Y\+(\d+)/, Enum.at(lines, 0), capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)

    # Parse Button B: X+22, Y+67
    [bx, by] =
      Regex.run(~r/Button B: X\+(\d+), Y\+(\d+)/, Enum.at(lines, 1), capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)

    # Parse Prize: X=8400, Y=5400
    [px, py] =
      Regex.run(~r/Prize: X=(\d+), Y=(\d+)/, Enum.at(lines, 2), capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)

    %Machine{ax: ax, ay: ay, bx: bx, by: by, px: px, py: py}
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec machine_tokens(Machine.t(), integer() | :infinity) :: integer() | nil
  defp machine_tokens(%{ax: ax, ay: ay, bx: bx, by: by, px: px, py: py}, max_uses) do
    with d <- ax * by - ay * bx,
         true <- d != 0,
         a <- div(px * by - py * bx, d),
         b <- div(py * ax - px * ay, d),
         true <- a >= 0,
         true <- b >= 0,
         true <- rem(px * by - py * bx, d) == 0,
         true <- rem(py * ax - px * ay, d) == 0,
         true <- a <= max_uses,
         true <- b <= max_uses do
      a * @a_price + b * @b_price
    else
      _ -> nil
    end
  end

  @spec solution1(input_type()) :: integer()
  def solution1(machines) do
    machines
    |> Enum.map(&machine_tokens(&1, @max_uses))
    |> Enum.filter(&(&1 != nil))
    |> Enum.sum()
  end

  @spec solution2(input_type()) :: integer()
  def solution2(machines) do
    machines
    |> Enum.map(fn %{px: px, py: py} = machine ->
      Map.merge(machine, %{px: px + @prize_error, py: py + @prize_error})
    end)
    |> Enum.map(&machine_tokens(&1, :infinity))
    |> Enum.filter(&(&1 != nil))
    |> Enum.sum()
  end
end

defmodule AOC.Day14 do
  @width 101
  @height 103
  @seconds 100

  defstruct [:x, :y, :vx, :vy]

  @type robot :: %__MODULE__{x: integer(), y: integer(), vx: integer(), vy: integer()}
  @type input_type :: [robot()]

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    text_input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_robot_line/1)
  end

  defp parse_robot_line(line) do
    # Parse line like "p=82,54 v=14,-84"
    [_, x, y, vx, vy] = Regex.run(~r/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/, line)

    %__MODULE__{
      x: String.to_integer(x),
      y: String.to_integer(y),
      vx: String.to_integer(vx),
      vy: String.to_integer(vy)
    }
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec move_robot(robot(), integer(), integer(), integer()) :: robot()
  def move_robot(robot, seconds, width, height) do
    %{robot | x: Integer.mod(robot.x + robot.vx * seconds, width), y: Integer.mod(robot.y + robot.vy * seconds, height)}
  end

  def quadrant_sizes(robots, width, height) do
    mid_x = div(width, 2)
    mid_y = div(height, 2)

    Enum.reduce(robots, %{q1: 0, q2: 0, q3: 0, q4: 0}, fn robot, acc ->
      quadrant =
        cond do
          robot.x == mid_x or robot.y == mid_y -> nil
          robot.x < mid_x and robot.y < mid_y -> :q1
          robot.x > mid_x and robot.y < mid_y -> :q2
          robot.x < mid_x and robot.y > mid_y -> :q3
          robot.x > mid_x and robot.y > mid_y -> :q4
        end

      if quadrant do
        Map.update!(acc, quadrant, &(&1 + 1))
      else
        acc
      end
    end)
  end

  @spec solution1(input_type()) :: integer()
  def solution1(robots) do
    solution1(robots, @width, @height, @seconds)
  end

  @spec solution1(input_type(), integer(), integer(), integer()) :: integer()
  def solution1(robots, width, height, seconds) do
    robots
    |> Enum.map(&move_robot(&1, seconds, width, height))
    |> quadrant_sizes(width, height)
    |> Enum.map(fn {_quadrant, size} -> size end)
    |> Enum.product()
  end

  @spec print_robots(input_type(), integer(), integer(), integer()) :: :ok
  def print_robots(robots, width, height, index) do
    grid = :array.new(width * height, default: ".")

    robot_positions =
      Enum.reduce(robots, grid, fn %{x: x, y: y}, acc ->
        :array.set(y * width + x, "R", acc)
      end)

    File.open("robots_output.txt", [:append], fn file ->
      for y <- 0..(height - 1) do
        row = for x <- 0..(width - 1), do: :array.get(y * width + x, robot_positions)
        IO.write(file, Enum.join(row, "") <> "\n")
      end

      IO.write(file, "--- #{index} ---\n")
    end)
  end

  @spec contains_tree_pattern?(input_type(), integer(), integer()) :: boolean()
  def contains_tree_pattern?(robots, width, height) do
    # Create a set of robot positions for O(1) lookup
    robot_positions = MapSet.new(robots, fn %{x: x, y: y} -> {x, y} end)

    # Look for horizontal lines of robots (tree border pattern)
    # Check each row for consecutive robots
    Enum.any?(0..(height - 1), fn y ->
      has_long_horizontal_line?(robot_positions, y, width)
    end)
  end

  # Check if a row has a long horizontal line of robots (at least 30 consecutive)
  defp has_long_horizontal_line?(robot_positions, y, width) do
      0..(width - 1)
      |> Enum.reduce({0, 0}, fn x, {current_streak, max_streak} ->
        if MapSet.member?(robot_positions, {x, y}) do
          new_streak = current_streak + 1
          {new_streak, max(max_streak, new_streak)}
        else
          {0, max_streak}
        end
      end)
    |> elem(1)
    |> then(&(&1 >= 30))
  end

  @spec solution2(input_type()) :: integer()
  def solution2(robots) do
    # Vertical patterns start at 16 seconds and repeat every 103 seconds
    offset = 16
    multi = 103

    robots
    |> Enum.map(&move_robot(&1, offset, @width, @height))
    |> Stream.iterate(fn robots -> Enum.map(robots, &move_robot(&1, multi, @width, @height)) end)
    |> Stream.with_index()
    |> Stream.filter(fn {robots, _index} -> contains_tree_pattern?(robots, @width, @height) end)
    |> Stream.take(1)
    |> Stream.map(fn {_robots, index} -> multi * index + offset end)
    |> Enum.at(0)
  end
end

# ......................................RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR................................
# ...........R..........................R.............................R................................
# ......................................R.............................R................................
# ......................................R.............................R..R....R........................
# ........R.............................R.............................R...........................R....
# ......................................R..............R..............R....................R...........
# .......................R..............R.............RRR.............R................................
# ......................................R............RRRRR............R........R.......................
# ......................................R...........RRRRRRR...........R................................
# .....R................................R..........RRRRRRRRR..........R................................
# ......................................R............RRRRR............R......R.........................
# ......................................R...........RRRRRRR...........R................................
# R.....................................R..........RRRRRRRRR..........R................................
# ......................................R.........RRRRRRRRRRR.........R....R...........................
# ......R...........R...................R........RRRRRRRRRRRRR........R...................R..R.........
# ......................................R..........RRRRRRRRR..........R................................
# .R....................................R.........RRRRRRRRRRR.........R.....R.R...................R....
# ......................................R........RRRRRRRRRRRRR........R................................
# ........R...........R.................R.......RRRRRRRRRRRRRRR.......R...............R................
# .......R.......R......................R......RRRRRRRRRRRRRRRRR......R................................
# ............................R.........R........RRRRRRRRRRRRR........R......................R....R....
# ..............R.......................R.......RRRRRRRRRRRRRRR.......R................................
# ........................R.............R......RRRRRRRRRRRRRRRRR......R....................R...........
# R.R...................................R.....RRRRRRRRRRRRRRRRRRR.....R...R............................
# ......................................R....RRRRRRRRRRRRRRRRRRRRR....R...........R....................
# ..............................R.......R.............RRR.............R.R..............................
# ......................................R.............RRR.............R................................
# ......................................R.............RRR.............R.................R..............
# ..................R.......R....R......R.............................R................................
# ......................................R.............................R................................
# ......................................R.............................R.........R.....R................
# ......................................R.............................R................................
# ......................................RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR................................

defmodule Utils do
  defmacro __using__(_) do
    quote do
      import Utils
    end
  end

  @type int_list_pair :: {list(integer()), list(integer())}

  @doc """
  Counts the occurrences of each element in the given list.

  ## Parameters
    - list: A list of elements to be counted.

  ## Returns
    - A map where the keys are the elements from the list and the values are the counts of those elements.

  ## Examples

      iex> counter([:a, :b, :a, :c, :b, :a])
      %{a: 3, b: 2, c: 1}

      iex> counter([1, 2, 2, 3, 3, 3])
      %{1 => 1, 2 => 2, 3 => 3}

  """
  @spec counter([a]) :: %{a => integer()} when a: any()
  def counter(list) do
    Enum.reduce(list, %{}, fn element, acc ->
      Map.update(acc, element, 1, &(&1 + 1))
    end)
  end

  @doc """
  Transposes a list of lists (matrix).

  ## Parameters
    - m: A list of lists representing the matrix to be transposed.

  ## Returns
    - A new list of lists where the rows and columns of the input matrix are swapped.

  ## Examples

    iex> transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

    iex> transpose([[1, 2], [3, 4], [5, 6]])
    [[1, 3, 5], [2, 4, 6]]
  """
  @spec transpose([list(a)]) :: [list(a)] when a: any()
  def transpose(m) do
    Enum.zip_with(m, &Function.identity/1)
  end

  @doc """
    Circle shift a list by a given number of positions in O(n) time.

  An implementation of the algorithm described in Jon Bentley's "Programming Pearls 2nd Edition".

  ## Examples

      iex> shift_left([1, 2, 3, 4], 1)
      [2, 3, 4, 1]

      iex> shift_left([1, 2, 3, 4], 2)
      [3, 4, 1, 2]

      iex> shift_left([1, 2, 3, 4], 3)
      [4, 1, 2, 3]

      iex> shift_left([1, 2, 3, 4], 6)
      [1, 2, 3, 4]

      iex> shift_left([1, 2, 3, 4], -1)
      [4, 1, 2, 3]
  """

  def shift_left(list, n) when n < 0, do: shift_left(list, length(list) + n)

  def shift_left(list, n) do
    size = Enum.count(list)

    list
    |> Enum.reverse_slice(n, size)
    |> Enum.reverse_slice(0, n)
    |> Enum.reverse_slice(0, size)
  end
end

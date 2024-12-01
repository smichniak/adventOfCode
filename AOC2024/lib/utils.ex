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
end

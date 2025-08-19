defmodule AOC.Day09 do
  use Utils

  @type int_map :: Utils.int_map()
  @type input_type :: {int_map(), [integer()]}

  @spec disk_block(integer()) :: integer() | nil
  def disk_block(index) do
    if rem(index, 2) == 0 do
      div(index, 2)
    else
      nil
    end
  end

  @spec parser1(binary()) :: input_type()
  def parser1(text_input) do
    nums =
      text_input
      |> String.trim()
      |> String.split("", trim: true)
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()

    disk =
      nums
      |> Enum.flat_map(fn {v, i} ->
        if v > 0 do
          for _x <- 1..v, do: disk_block(i)
        else
          []
        end
      end)

    file_sizes =
      nums
      |> Enum.take_every(2)
      |> Enum.into(%{}, fn {size, index} -> {div(index, 2), size} end)

    {disk, file_sizes}
  end

  @spec parser2(binary()) :: input_type()
  def parser2(text_input), do: parser1(text_input)

  @spec defragment(nil | integer(), {int_map(), [integer()]}) :: {nil | integer(), {int_map(), [integer()]}}
  def defragment(_block, {file_sizes, []}), do: {nil, {file_sizes, []}}

  def defragment(block, {file_sizes, [file_id | file_ids_t] = file_ids}) do
    case Map.fetch!(file_sizes, file_id) do
      0 -> defragment(block, {file_sizes, file_ids_t})
      _ -> defragment_final(block, {file_sizes, file_ids})
    end
  end

  def defragment_final(nil, {file_sizes, [last_file | _file_ids_t] = file_ids}),
    do: {last_file, {Map.update!(file_sizes, last_file, &(&1 - 1)), file_ids}}

  def defragment_final(file_id, {file_sizes, file_ids}),
    do: {file_id, {Map.update!(file_sizes, file_id, &(&1 - 1)), file_ids}}

  def check_sum({nil, _index}), do: 0
  def check_sum({file_id, index}), do: file_id * index

  @spec solution1(input_type()) :: integer()
  def solution1({disk, file_sizes}) do
    file_ids = Map.keys(file_sizes) |> Enum.sort(:desc)

    new_disk =
      disk
      |> Enum.map_reduce({file_sizes, file_ids}, &defragment/2)
      |> elem(0)

    new_disk
    |> Enum.with_index()
    |> Enum.map(&check_sum/1)
    |> Enum.sum()
  end

  @spec solution2(input_type()) :: integer()
  def solution2({disk, file_sizes}) do
    # Create initial file and free space structures
    {files, free_spaces} = parse_files_and_spaces(disk)

    # Get file IDs in decreasing order
    file_ids = Map.keys(file_sizes) |> Enum.sort(:desc)

    # Move files according to the new algorithm
    final_files = move_files(files, free_spaces, file_ids, file_sizes)

    # Calculate checksum
    calculate_checksum_part2(final_files)
  end

  # Parse the disk into files and free spaces with positions
  defp parse_files_and_spaces(disk) do
    disk
    |> Enum.with_index()
    |> Enum.reduce({%{}, []}, fn {block, pos}, {files, free_spaces} ->
      case block do
        nil ->
          # Free space - try to merge with previous free space
          case free_spaces do
            [{prev_pos, prev_size} | rest] when prev_pos + prev_size == pos ->
              {files, [{prev_pos, prev_size + 1} | rest]}
            _ ->
              {files, [{pos, 1} | free_spaces]}
          end
        file_id ->
          # File block - update file info
          case Map.get(files, file_id) do
            nil ->
              {Map.put(files, file_id, {pos, 1}), free_spaces}
            {start_pos, size} ->
              {Map.put(files, file_id, {start_pos, size + 1}), free_spaces}
          end
      end
    end)
    |> then(fn {files, free_spaces} -> {files, Enum.reverse(free_spaces)} end)
  end

  # Move files according to part 2 algorithm
  defp move_files(files, free_spaces, file_ids, file_sizes) do
    Enum.reduce(file_ids, {files, free_spaces}, fn file_id, {current_files, current_free_spaces} ->
      file_size = Map.get(file_sizes, file_id, 0)
      {file_pos, _} = Map.get(current_files, file_id, {0, 0})

      # Find leftmost free space that can fit this file and is to the left of current position
      case find_suitable_free_space(current_free_spaces, file_size, file_pos) do
        nil ->
          # No suitable space found, file doesn't move
          {current_files, current_free_spaces}
        {space_pos, space_size, remaining_spaces} ->
          # Move the file and update free spaces
          new_files = Map.put(current_files, file_id, {space_pos, file_size})

          # Add remaining free space if any
          new_free_spaces =
            if space_size > file_size do
              insert_free_space(remaining_spaces, {space_pos + file_size, space_size - file_size})
            else
              remaining_spaces
            end

          # Add the old file position as free space
          final_free_spaces = insert_free_space(new_free_spaces, {file_pos, file_size})

          {new_files, final_free_spaces}
      end
    end)
    |> elem(0)
  end

  # Find the leftmost free space that can fit the file and is to the left of file_pos
  defp find_suitable_free_space(free_spaces, file_size, file_pos) do
    free_spaces
    |> Enum.with_index()
    |> Enum.find(fn {{space_pos, space_size}, _} ->
      space_pos < file_pos and space_size >= file_size
    end)
    |> case do
      nil -> nil
      {{space_pos, space_size}, index} ->
        remaining_spaces = List.delete_at(free_spaces, index)
        {space_pos, space_size, remaining_spaces}
    end
  end

  # Insert a free space maintaining sorted order by position and merging adjacent spaces
  defp insert_free_space(free_spaces, {pos, size}) do
    # Add the new space and sort
    all_spaces = [{pos, size} | free_spaces] |> Enum.sort_by(fn {p, _} -> p end)

    # Merge adjacent spaces
    merge_adjacent_spaces(all_spaces)
  end

  defp merge_adjacent_spaces([]), do: []
  defp merge_adjacent_spaces([space]), do: [space]
  defp merge_adjacent_spaces([{pos1, size1}, {pos2, size2} | rest]) do
    if pos1 + size1 == pos2 do
      # Merge these two spaces
      merge_adjacent_spaces([{pos1, size1 + size2} | rest])
    else
      # Keep the first space and continue
      [{pos1, size1} | merge_adjacent_spaces([{pos2, size2} | rest])]
    end
  end

  # Calculate checksum for part 2
  defp calculate_checksum_part2(files) do
    files
    |> Enum.flat_map(fn {file_id, {start_pos, size}} ->
      for i <- 0..(size - 1), do: {file_id, start_pos + i}
    end)
    |> Enum.map(fn {file_id, pos} -> file_id * pos end)
    |> Enum.sum()
  end
end

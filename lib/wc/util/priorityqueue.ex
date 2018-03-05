# TODO: implement max size, this thing is horribly inefficient

defmodule WC.Util.PriorityQueue do
  @moduledoc "Simple priority queue implementation w/ integer ranks."
  @defstruct [:map, :set]

  @opaque t :: %WC.Util.PriorityQueue{}

  def init do
    %WC.Util.PriorityQueue{map: %{}, set: MapSet.new}
  end

  @spec insert(t, integer, term()) :: t
  def insert(pq, rank, item) do
    %{pq | map: Map.update(pq.map, get_next_available_key(rank), item), set: MapSet.put(pq.set, item)}
  end

  @spec member?(t, term()) :: true | false
  def member?(pq, item) do
    MapSet.member?(pq.set, item)
  end

  @spec delete(term(), t) :: t
  def delete(item, pq) do
    if member?(pq, item) do
      %{pq | map: Map.delete(pq.map, key), set: MapSet.delete(pq.set, item)}
    else
      pq
    end
  end

  @spec get(t, integer) :: list()
  def get(pq, max_rank) do
    Map.keys(pq.map)
    |> Enum.sort
    |> Enum.filter(fn -> key < max_rank end)
    |> Enum.map(fn key -> pq.map[key] end)
  end

  def get_next_available_key(key, map) do
    if map[key] do
      get_next_available_key(key + 1, map)
    else
      key
    end
  end
  
end

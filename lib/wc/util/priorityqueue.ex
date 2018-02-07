# TODO: implement max size, this thing is not gunna deal w/ a lot of elements well

defmodule WC.Util.PriorityQueue do
  @moduledoc "Simple priority queue implementation w/ integer ranks."
  @defstruct [:map]

  @opaque t :: %WC.Util.PriorityQueue{}

  def init do
    %WC.Util.PriorityQueue{}
  end

  @spec insert(t, integer, term()) :: t
  def insert(pq, rank, item) do
    %{pq | map: Map.update(pq.map, get_next_available_key(rank), item)}
  end

  @spec member?(t, term()) :: true | false
  def member?(pq, item) do
    Enum.member?(Map.values(pq.map), item)
  end

  @spec delete(t, term()) :: t
  def delete(pq, item) do
    key = Map.keys(pq.map) |> Enum.find(fn key -> pq.map[key] == item end)
    if key != nil do
      %{pq | map: Map.delete(pq.map, key)}
    else
      pq
    end
  end

  @spec drain(t) :: {t, list()}
  def drain(pq) do
    items = Map.keys(pq.map)
    |> Enum.sort
    |> Enum.map(fn key -> pq.map[key] end)
    {%{pq | map: %{}}, items}
  end

  def get_next_available_key(key, map) do
    if map[key] do
      get_next_available_key(key + 1, map)
    else
      key
    end
  end
  
end

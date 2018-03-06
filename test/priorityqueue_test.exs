alias WC.Util.PriorityQueue, as: PriorityQueue

defmodule PriorityQueueTest do
  use ExUnit.Case
  doctest WC

  test "Can insert and retrieve single item" do
    pq = PriorityQueue.new
    ts = :os.system_time(:millisecond)
    pq2 = PriorityQueue.insert(pq, ts, :test)
    assert PriorityQueue.member?(pq2, :test)
    assert [:test] = PriorityQueue.get(pq2, ts+1)
  end

  test "Can insert multiple items w/ same rank" do
    pq = PriorityQueue.new
    ts = :os.system_time(:millisecond)
    pq2 = PriorityQueue.insert(pq, ts, :test1)
    |> PriorityQueue.insert(ts, :test2)
    |> PriorityQueue.insert(ts, :test3)
    assert match?([], PriorityQueue.get(pq2, ts))    
    assert match?([:test1], PriorityQueue.get(pq2, ts+1))
    assert match?([:test1, :test2], PriorityQueue.get(pq2, ts+2))
    assert match?([:test1, :test2, :test3], PriorityQueue.get(pq2, ts+3))
  end

  test "Can remove items" do
    ts = :os.system_time(:millisecond)
    pq = PriorityQueue.new
    |> PriorityQueue.insert(ts, :test)
    assert match?([:test], PriorityQueue.get(pq, ts+1))
    pq2 = PriorityQueue.delete(:test, pq)
    assert match?([], PriorityQueue.get(pq2, ts+1))
    assert false = PriorityQueue.member?(:test)
  end

end

alias MBC.Blockchain.TX, as: TX

defmodule MBC.Blockchain.MempoolTable do

  def init() do
    :mnesia.create_table(Mempool, [attributes: [:tx_hash, :tx]])
  end

  def insert(tx = %TX{}) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({Mempool, TX.hash(tx), tx}) end)
    result
  end

  def get_all do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.match_object({Mempool, :_, :_}) end)
    result
  end

  def clear do
    {:atomic, result} = :mnesia.clear_table(Mempool)
    result
  end

  def get_all_and_clear do
    {:atomic, result} = :mnesia.transaction(fn ->
      result = :mnesia.match_object({Mempool, :_, :_})
      :mnesia.clear_table(Mempool)
      result
    end)
    result
  end
  
end

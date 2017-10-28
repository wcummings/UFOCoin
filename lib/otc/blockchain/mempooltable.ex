alias OTC.Blockchain.TX, as: TX

defmodule OTC.Blockchain.MempoolTable do

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
  
end

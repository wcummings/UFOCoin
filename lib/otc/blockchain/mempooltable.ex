defmodule OTC.Blockchain.MempoolTable do

  def init() do
    :mnesia.create_table(Mempool, [attributes: []])
  end
  
end

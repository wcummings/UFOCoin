alias OTC.Blockchain.Block, as: Block

defmodule OTC.Blockchain.BlockTable do

  def init do
    :mnesia.create_table(Block, [:attributes, [:block_hash, :block]])
  end

  def insert(block) do
    {:atomic, result} = :mnesia.write({Block, Block.hash(block), block})
    result
  end
  
end

alias MBC.Blockchain.Block, as: Block

defmodule MBC.Blockchain.BlockTable do

  def init do
    :mnesia.create_table(Block, [:attributes, [:block_hash, :block]])
  end

  def insert(block) do
    {:atomic, result} = :mnesia.write({Block, Block.hash(block), block})
    result
  end
  
end

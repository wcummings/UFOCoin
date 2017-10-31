alias MBC.Blockchain.Block, as: Block

defmodule MBC.Blockchain.BlockTable do

  def init do
    :mnesia.create_table(Block, [:attributes, [:block_hash, :height, :block, index: [:height]]])
  end

  def insert(block) do
    # f = fn ->
    #   :mnesia.read({Block,

    # 		    result
    # 		    # {:atomic, result} = :mnesia.write({Block, Block.hash(block), block})
  end

  def get(block_hash) do
    case :mnesia.transaction(fn -> :mnesia.read({Block, block_hash}) end) do
      {:atomic, []} -> :undefined
      {:atomic, [block]} -> block
    end
  end
  
end

alias MBC.Blockchain.BlockTable, as: BlockTable
alias MBC.Blockchain.Block, as: Block

defmodule BlockTableTest do
  use ExUnit.Case
  doctest MBC

  test "Can find longest chain" do
    BlockTable.init()
    BlockTable.insert(MBC.genesis_block)

    next_block = %Block{prev_block_hash: Block.hash(MBC.genesis_block), difficulty: 1, height: MBC.genesis_block.height + 1, timestamp: :os.system_time(:millisecond)}
    BlockTable.insert(next_block)

    next_block_2 = %Block{prev_block_hash: Block.hash(next_block), difficulty: 1, height: next_block.height + 1, timestamp: :os.system_time(:millisecond)}
    BlockTable.insert(next_block_2)
    
    assert [next_block_2] == BlockTable.get_longest
  end

  test "Can read TX by hash" do
    BlockTable.init()
    block = MBC.genesis_block()
    BlockTable.insert(block)
    hash = Block.hash(block)
    assert block == BlockTable.get(hash)
  end
  
end

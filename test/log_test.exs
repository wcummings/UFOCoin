alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.Log, as: BlockchainLog

defmodule LogTest do
  use ExUnit.Case
  doctest MBC

  test "Can insert and read blocks" do
    path = MBC.Util.temp_file()
    {:ok, log} = BlockchainLog.init(path)
    encoded_block = Block.encode(MBC.genesis_block)
    {:ok, offset} = BlockchainLog.append_block(log, encoded_block)
    assert match?({^encoded_block, _}, BlockchainLog.read_block(log, offset))
    block2 = %Block{prev_block_hash: Block.hash(encoded_block), difficulty: 20, height: 1, timestamp: :os.system_time(:millisecond)}
    encoded_block2 = Block.encode(block2)
    {:ok, offset2} = BlockchainLog.append_block(log, encoded_block2)
    assert match?({^encoded_block2, _}, BlockchainLog.read_block(log, offset2))
  end
  
end

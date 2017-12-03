alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Log, as: BlockchainLog

defmodule LogTest do
  use ExUnit.Case
  doctest WC

  test "Can insert and read blocks" do
    path = WC.Util.temp_file()
    {:ok, log} = BlockchainLog.init(path)
    encoded_block = Block.encode(WC.genesis_block)
    offset = BlockchainLog.append_block(log, encoded_block)
    assert match?({:ok, {^encoded_block, _}}, BlockchainLog.read_block(log, offset))
    block2 = %Block{header: %BlockHeader{prev_block_hash: Block.hash(encoded_block), difficulty: 20, height: 1, timestamp: :os.system_time(:millisecond)}, txs: []}
    encoded_block2 = Block.encode(block2)
    offset2 = BlockchainLog.append_block(log, encoded_block2)
    assert match?({:ok, {^encoded_block2, _}}, BlockchainLog.read_block(log, offset2))
  end
  
end

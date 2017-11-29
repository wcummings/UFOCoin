alias MBC.Blockchain.BlockHeader, as: BlockHeader
alias MBC.Blockchain.Block, as: Block

defmodule BlockHeaderTest do
  use ExUnit.Case
  doctest MBC

  test "Can encode and decode" do
    block_header = MBC.genesis_block.header
    encoded_block_header = BlockHeader.encode(block_header)
    decoded_block_header = BlockHeader.decode(encoded_block_header)
    assert block_header == decoded_block_header
  end

  test "Update nonce nop" do
    block_header = BlockHeader.encode(MBC.genesis_block.header)
    block_header2 = Block.update_nonce(block_header, <<0, 0, 0, 0>>)
    assert block_header == block_header2
  end
  
end

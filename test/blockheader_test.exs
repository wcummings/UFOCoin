alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule BlockHeaderTest do
  use ExUnit.Case
  doctest WC

  test "Can encode and decode" do
    block_header = WC.genesis_block.header
    encoded_block_header = BlockHeader.encode(block_header)
    decoded_block_header = BlockHeader.decode(encoded_block_header)
    assert block_header == decoded_block_header
  end

  test "Update nonce nop" do
    block_header = BlockHeader.encode(WC.genesis_block.header)
    block_header2 = BlockHeader.update_nonce(block_header, <<0, 0, 0, 0>>)
    assert block_header == block_header2
  end
  
end

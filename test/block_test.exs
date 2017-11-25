alias MBC.Blockchain.Block, as: Block

defmodule BlockTest do
  use ExUnit.Case
  doctest MBC

  test "Can encode and decode" do
    block = MBC.genesis_block
    encoded_block = Block.encode(block)
    decoded_block = Block.decode(encoded_block)
    assert block == decoded_block
  end

  test "Update nonce nop" do
    block = Block.encode(MBC.genesis_block)
    block2 = Block.update_nonce(block, <<0, 0, 0, 0>>)
    assert block == block2
  end
  
end

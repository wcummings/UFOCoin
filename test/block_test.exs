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

end

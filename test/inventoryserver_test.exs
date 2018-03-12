alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.LogServer, as: LogServer

# FIXME: wrong test name
defmodule InventoryServerTest do
  use ExUnit.Case
  doctest WC

  test "Block locator" do
     block_hashes = LogServer.get_block_locator()
     assert length(block_hashes) == 1
     last_hash = Enum.at(block_hashes, 0)
     assert last_hash == Block.hash(WC.genesis_block)
  end
  
end

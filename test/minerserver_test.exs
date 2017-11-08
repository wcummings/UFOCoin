alias MBC.Blockchain.TX, as: TX
alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.Output, as: Output

defmodule MinerServerTest do
  use ExUnit.Case
  doctest MBC

  test "Can mine a block" do
    # coinbase = %TX{inputs: [], outputs: [%Output{amount: 1000, destination: <<0::32>>}]}
    block = %Block{prev_block_hash: <<0 :: size(32)>>, difficulty: 20, txs: [], timestamp: :os.system_time(:millisecond)}
    hash = MBC.Miner.MinerServer.mine(block)
  end
  
end

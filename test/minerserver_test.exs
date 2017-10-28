alias OTC.Blockchain.TX, as: TX
alias OTC.Blockchain.Block, as: Block
alias OTC.Blockchain.Output, as: Output

defmodule MinerServerTest do
  use ExUnit.Case
  doctest OTC

  test "Can mine a block" do
    coinbase = %TX{inputs: [], outputs: [%Output{amount: 1000, destination: <<0::32>>}]}
    block = %Block{prev_block_hash: <<0::32>>, difficulty: 1, txs: [coinbase], timestamp: :os.system_time(:millisecond)}
    hash = OTC.Miner.MinerServer.mine(block)
  end
  
end

alias OTC.Blockchain.Block, as: Block

require Logger

defmodule OTC.Miner.MinerServer do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    {:ok, %{}}
  end

  def mine(block) do
    target = OTC.Util.difficulty_to_target(block.difficulty)
    mine(block, target)
  end
  
  def mine(block, target) do
    nonce = :crypto.strong_rand_bytes(4)
    block_with_nonce = %{block | nonce: nonce}
    case Block.check_nonce(block_with_nonce) do
      {true, hash} ->
	Logger.info "Successfully mined block, difficulty = #{block.difficulty}, nonce = #{nonce}, block_hash = #{Base.encode16(hash)}"
	block_with_nonce
      {false, hash} ->
	# Logger.info "Invalid block_hash = #{Base.encode16(hash)}"
	mine(block, target)
    end
  end

end

alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.MempoolTable, as: MempoolTable

require Logger

defmodule MBC.Miner.MinerServer do
  use GenServer

  @initial_state %{pids: [], proc_count: nil}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    proc_count = Application.get_env(:otc, :mining_proc_count, 1)
    {:ok, %{@initial_state | proc_count: proc_count}}
  end

  def handle_cast(:new_block, state) do
    txs = MempoolTable.get_all_and_clear
    # TODO
    {:noreply, state}
  end
  
  def mine(block) do
    mine(block, MBC.Util.difficulty_to_target(block.difficulty))
  end

  def mine(block = %Block{}, target) do
    mine(Block.encode(%{block | nonce: :crypto.strong_rand_bytes(4)}), target)
  end

  def mine(block, target) when is_binary(block) do
    case Block.check_nonce(block) do
      {true, hash} ->
	Logger.info "Successfully mined block, difficulty = #{target}, block_hash = #{Base.encode16(hash)}"
	block
      {false, hash} ->
	# Logger.info "Invalid block_hash = #{Base.encode16(hash)}"
	mine(Block.update_nonce(block, :crypto.strong_rand_bytes(4)), target)
    end
  end

end

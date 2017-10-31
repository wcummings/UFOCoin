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
    target = MBC.Util.difficulty_to_target(block.difficulty)
    mine(block, target)
  end

  # PoC, slow as fuck
  def mine(block, target) do
    nonce = :crypto.strong_rand_bytes(4)
    block_with_nonce = %{block | nonce: nonce}
    case Block.check_nonce(block_with_nonce) do
      {true, hash} ->
	Logger.info "Successfully mined block, difficulty = #{block.difficulty}, nonce = #{:crypto.bytes_to_integer(nonce)}, block_hash = #{Base.encode16(hash)}"
	block_with_nonce
      {false, hash} ->
	# Logger.info "Invalid block_hash = #{Base.encode16(hash)}"
	mine(block, target)
    end
  end

end

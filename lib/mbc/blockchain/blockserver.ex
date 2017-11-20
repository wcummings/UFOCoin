alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockTable, as: BlockTable
alias MBC.Miner.MinerServer, as: MinerServer

defmodule MBC.Blockchain.BlockServer do
  use GenServer

  @initial_state %{chain_height: nil}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def accept_block(new_block) do
    GenServer.call(__MODULE__, {:accept_block, new_block})
  end

  def init([]) do
    [%Block{height: height}|_] = BlockTable.get_longest
    {:ok, %{@initial_state | chain_height: height}}
  end
  
  def handle_call({:accept_block, new_block}, state = %{chain_height: chain_height}) do
    case check_block(new_block) do
      :ok ->
	Logger.info "New block accepted: #{inspect(new_block)}"
	BlockTable.insert(new_block)
	# If the node receive multiple blocks with valid pow, the same prev_block_hash and height,
	# it will continue mining the first it received
	if new_block.height > chain_height do
	  MinerServer.new_block
	end
	{:reply, :ok, %{state | chain_height: new_block.height}}
      {:error, error} ->
	{:reply, {:error, error}, state}
  end

  def check_block(new_block) do
    if Block.check_nonce(new_block) do
      check_prev_block(new_block)
    else
      {:error, :badnonce}
    end
  end

  def check_prev_block(block = %Block{prev_block_hash: prev_block_hash}) do
    case BlockTable.get(prev_block_hash) do
      :undefined ->
	{:error, {:prevblocknotfound, prev_block_hash}}
      prev_block ->
	check_prev_block(block, prev_block)
    end
  end

  def check_prev_block(new_block = %Block{height: height}, %Block{height: prev_height}) do
    if (height - 1) == prev_height do
      :ok
    else
      {:error, :orphanblock}
    end
  end
  
end

require Logger

alias MBC.Blockchain.Log, as: BlockchainLog
alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias MBC.Miner.MinerServer, as: MinerServer
alias MBC.Blockchain.BlockHashIndex, as: BlockHashIndex

# FIXME: should this be an FSM?

defmodule MBC.Blockchain.LogServer do
  use GenServer

  @initial_state %{log: nil, index_complete: false, tip: nil}

  @default_blockchain_log_path "/var/mbc/blocks.dat"
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def accept_block(block) do
    GenServer.call(__MODULE__, {:accept_block, block})
  end
  
  def init([]) do
    {:ok, log} = BlockchainLog.init(@default_blockchain_log_path)
    parent = self()
    spawn_link fn ->
      tip = BlockchainLog.index_blocks(log)
      send parent, {:index_complete, tip}
    end
    {:ok, %{@initial_state | log: log}}
  end

  def handle_call(_, _from, state = %{index_complete: false}) do
    {:reply, {:error, :index_incomplete}, state}
  end

  def handle_call({:accept_block, block}, _from, state = %{log: log, tip: tip}) do
    encoded_block = Block.encode(block)
    block_hash = Block.hash(encoded_block)
    case check_block(log, block) do
      :ok ->
	Logger.info "New block accepted: #{inspect(block)}"
	if block.height > tip.height do
	  MinerServer.new_block(tip)
	end
	{:ok, offset} = BlockchainLog.append_block(log, encoded_block)
	:ok = BlockHashIndex.insert(block_hash, offset)
	{:reply, :ok, state}
      {:error, error} ->
	{:reply, {:error, error}, state}
    end
  end

  def handle_info({:index_complete, tip}, state) do
    # FIXME: sync inventory before node starts mining
    MinerServer.new_block(tip)
    {:noreply, %{state | index_complete: true, tip: tip}}
  end
  
  def check_block(log, new_block) do
    if Block.check_nonce(new_block) do
      check_prev_block(log, new_block)
    else
      {:error, :badnonce}
    end
  end

  def check_prev_block(log, block = %Block{prev_block_hash: prev_block_hash}) do
    case BlockHashIndex.get_offset(prev_block_hash) do
      :undefined ->
	{:error, :orphan}
      offset ->
	{:ok, {encoded_prev_block, _}} = BlockchainLog.read_block(log, offset)
	prev_block = Block.decode(encoded_prev_block)
	check_prev_block(block, prev_block)
    end
  end

  def check_prev_block(%Block{height: height}, %Block{height: prev_height}) do
    if (height - 1) == prev_height do
      :ok
    else
      {:error, :badheight}
    end
  end
  
end

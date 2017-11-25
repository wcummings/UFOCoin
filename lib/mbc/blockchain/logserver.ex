alias MBC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.Log, as: BlockchainLog

defmodule MBC.Blockchain.LogServer do
  use GenServer

  @initial_state %{tip: nil, index_complete: false, log: nil}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    log = BlockchainLog.init
    spawn_link index_blocks(log, self())
    {:ok, %{@initial_state | log: log}}
  end

  def get_block_by_hash(block_hash) do
    GenServer.call(__MODULE__, {:get_block_by_hash, block_hash})
  end
  
  def get_tip do
    GenServer.call(__MODULE__, :get_tip)
  end
  
  def update(block, offset) do
    GenServer.cast(__MODULE__, {:new_block, block, offset})
  end

  def handle_call(_, state = %{index_complete: false}) do
    {:reply, {:error, :index_incomplete}, state}
  end
  
  def handle_call({:get_block_by_hash, block_hash}, state = %{log: log}) do
    case BlockHashIndex.get_offset(block_hash) do
      :undefined ->
	{:reply, {:error, :notfound}, state}
      offset ->
	{encoded_block, _} = BlockchainLog.read_block(log, offset)
	block = Block.decode(encoded_block)
	{:reply, {:ok, block}, state}
    end
  end
  
  def handle_call(:get_tip, state = %{tip: tip}) do
    {:reply, {:ok, tip}, state}
  end
  
  def handle_cast({:update, block}, state = %{tip: tip, log: log}) do
    encoded_block = Block.encode(block)
    block_hash = Block.hash(block)
    offset = BlockchainLog.append_block(log, encoded_block)    
    :ok = BlockHashIndex.insert(block_hash, offset)
    new_tip = if block.height > tip.height do
      block
    else
      tip
    end
    {:noreply, %{state | tip: new_tip}}
  end

  def handle_info({:index_complete, tip}, state) do
    {:noreply, %{state | tip: tip, index_complete: true}}
  end

  def index_blocks(log, pid) do
    fn ->
      tip = BlockchainLog.index_blocks(log)
      send pid, {:index_complete, tip}
    end
  end
  
end

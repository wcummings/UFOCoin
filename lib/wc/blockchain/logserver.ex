require Logger

alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Log, as: BlockchainLog
alias WC.Miner.MinerServer, as: MinerServer

defmodule WC.Blockchain.LogServer do
  use GenServer

  @initial_state %{tip: nil, index_complete: false, log: nil}
  
  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    {:ok, log} = BlockchainLog.init
    if BlockchainLog.is_empty?(log) do
      Logger.info "Inserting genesis block..."
      BlockchainLog.append_block(log, Block.encode(WC.genesis_block))
    end
    spawn_link index_blocks(self())
    {:ok, %{@initial_state | log: log}}
  end

  @spec get_block_by_hash(BlockHeader.block_hash) :: {:ok, Block.t} | {:error, :notfound}
  def get_block_by_hash(block_hash) do
    GenServer.call(__MODULE__, {:get_block_by_hash, block_hash})
  end

  @spec get_tip() :: {:ok, Block.t}
  def get_tip do
    GenServer.call(__MODULE__, :get_tip)
  end

  @spec update(Block.t) :: :ok
  def update(block = %Block{}) do
    update(Block.encode(block))
  end

  @spec update(Block.encoded_block) :: :ok  
  def update(block) do
    GenServer.cast(__MODULE__, {:update, block})
  end

  def get_prev_blocks(number_of_blocks, tip) do
    get_prev_blocks(number_of_blocks, tip, [])
  end

  def get_prev_blocks(number_of_blocks, tip, blocks) do
    {:ok, block} = get_block_by_hash(tip.prev_block_hash)
    if (block.header.height == 0) or (tip.header.height - block.header.height == number_of_blocks) do
      blocks
    else
      get_prev_blocks(number_of_blocks, tip, [block|blocks])
    end
  end
  
  def handle_call(_, _, state = %{index_complete: false}) do
    {:reply, {:error, :index_incomplete}, state}
  end
  
  def handle_call({:get_block_by_hash, block_hash}, _from, state = %{log: log}) do
    {:reply, get_block_by_hash(log, block_hash), state}
  end
  
  def handle_call(:get_tip, _from, state = %{tip: tip}) do
    {:reply, {:ok, tip}, state}
  end
  
  def handle_cast({:update, encoded_block}, state = %{tip: tip, log: log}) do
    block = Block.decode(encoded_block)
    block_hash = Block.hash(encoded_block)
    offset = BlockchainLog.append_block(log, encoded_block)    
    :ok = BlockHashIndex.insert(block_hash, offset)
    new_tip = if block.header.height > tip.header.height do
      MinerServer.new_block(block)
      block      
    else
      tip
    end
    {:noreply, %{state | tip: new_tip}}
  end

  def handle_info({:index_complete, tip}, state) do
    Logger.info "Indexing complete, tip = #{inspect(tip)}"
    MinerServer.new_block(tip)
    {:noreply, %{state | tip: tip, index_complete: true}}
  end

  def index_blocks(pid) do
    fn ->
      # Can't share file handles across processes
      {:ok, log} = BlockchainLog.init
      Logger.info "Indexing blocks..."
      tip = BlockchainLog.index_blocks(log)
      send pid, {:index_complete, tip}
    end
  end

  def get_block_by_hash(log, block_hash) do
    case BlockHashIndex.get_offset(block_hash) do
      {:error, :notfound} ->
	{:error, :notfound}
      {:ok, offset} ->
	{:ok, {encoded_block, _}} = BlockchainLog.read_block(log, offset)
	block = Block.decode(encoded_block)
	{:ok, block}
    end
  end

end

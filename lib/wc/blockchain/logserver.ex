require Logger

alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias WC.Blockchain.PrevBlockHashIndex, as: PrevBlockHashIndex
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Log, as: BlockchainLog
alias WC.Blockchain.InvItem, as: InvItem
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
    GenServer.call(__MODULE__, {:get_block_with_index, BlockHashIndex, block_hash})
  end

  @spec get_blocks_by_prev_hash(BlockHeader.block_hash) :: {:ok, list(Block.t)} | {:error, :notfound}
  def get_blocks_by_prev_hash(prev_block_hash) do
    GenServer.call(__MODULE__, {:get_block_with_index, PrevBlockHashIndex, prev_block_hash})
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

  # Build a list of block hashes from newest to genesis, dense to start, then sparse
  @spec get_block_locator() :: list(InvItem.t)
  def get_block_locator do
    {:ok, tip} = get_tip()
    genesis_block_hash = Block.hash(WC.genesis_block)
    dense_hashes = for block <- get_prev_blocks(10, tip), do: Block.hash(block)
    if tip.header.height < 10 do
      dense_hashes ++ [genesis_block_hash]
    else
      dense_hashes ++ get_prev_block_hashes_sparse(tip) ++ [genesis_block_hash]
    end
    |> Enum.map(fn block_hash -> %InvItem{type: :block, hash: block_hash} end)
  end

  def get_prev_block_hashes_sparse(tip) do
    get_prev_block_hashes_sparse(tip, 1, 0, [])
  end
  
  def get_prev_block_hashes_sparse(tip, step, count, acc) do
    {:ok, block} = get_block_by_hash(tip.header.prev_block_hash)
    if block.header.height == 0 do
      acc
    else
      if count == step do
	get_prev_block_hashes_sparse(block, step * 2, 0, [Block.hash(block)|acc])
      else
	get_prev_block_hashes_sparse(block, step, count + 1, acc)
      end
    end
  end

  def get_next_blocks(number_of_blocks, block) do
    get_next_blocks(number_of_blocks, [block], [])
  end
  
  def get_next_blocks(number_of_blocks, _next_blocks, acc) when number_of_blocks <= 0 do
    Enum.take(acc, -number_of_blocks)
  end
  
  def get_next_blocks(number_of_blocks, next_blocks, acc) do
    next_blocks = Enum.map(next_blocks, fn block -> Block.hash(block) |> get_blocks_by_prev_hash end)
    |> Enum.filter(fn
      {:ok, _blocks} -> true
      _error -> false
    end)
    |> Enum.map(fn {:ok, blocks} -> blocks end)

    case next_blocks do
      [] ->
	get_next_blocks(0, [], acc)
      _ ->
	get_next_blocks(number_of_blocks - length(next_blocks), next_blocks, next_blocks ++ acc)
    end
  end
  
  def get_prev_blocks(number_of_blocks) do
    {:ok, tip} = get_tip()
    get_prev_blocks(number_of_blocks, tip)
  end
  
  def get_prev_blocks(number_of_blocks, tip) do
    get_prev_blocks(number_of_blocks, tip, [])
  end
  
  def get_prev_blocks(number_of_blocks, tip, acc) do
    {:ok, block} = get_block_by_hash(tip.header.prev_block_hash)
    if (block.header.height == 0) or (tip.header.height - block.header.height == number_of_blocks) do
      acc
    else
      get_prev_blocks(number_of_blocks, block, [block|acc])
    end
  end

  def handle_call(_, _, state = %{index_complete: false}) do
    {:reply, {:error, :index_incomplete}, state}
  end

  def handle_call({:get_block_with_index, index, block_hash}, _from, state = %{log: log}) do
    {:reply, get_block_with_index(log, index, block_hash), state}
  end
  
  def handle_call(:get_tip, _from, state = %{tip: tip}) do
    {:reply, {:ok, tip}, state}
  end
  
  def handle_cast({:update, encoded_block}, state = %{tip: tip, log: log}) do
    block = Block.decode(encoded_block)
    block_hash = Block.hash(encoded_block)
    offset = BlockchainLog.append_block(log, encoded_block)    
    :ok = BlockHashIndex.insert(block_hash, offset)
    :ok = PrevBlockHashIndex.insert(block.header.prev_block_hash, offset)
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

  def get_block_with_index(log, index, block_hash) do
    case index.get_offset(block_hash) do
      {:error, :notfound} ->
	{:error, :notfound}
      {:ok, offsets} when is_list(offsets) ->
	blocks = for offset <- offsets do
	    {:ok, {encoded_block, _}} = BlockchainLog.read_block(log, offset)
	    Block.decode(encoded_block)
	  end
	{:ok, blocks}
      {:ok, offset} ->
	{:ok, {encoded_block, _}} = BlockchainLog.read_block(log, offset)
	block = Block.decode(encoded_block)
	{:ok, block}
    end
  end

end

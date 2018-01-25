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

  @initial_state %{tip: nil, index_complete: false, log: nil, chain_index: nil}

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    {:ok, log} = BlockchainLog.init
    if BlockchainLog.is_empty?(log) do
      Logger.info "Inserting genesis block..."
      BlockchainLog.append_block(log, Block.encode(WC.genesis_block))
    end
    chain_index = :ets.new(:chain_index, [:set, :protected])    
    spawn_link index_blocks(self())
    {:ok, %{@initial_state | log: log, chain_index: chain_index}}
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

  # @spec get_next_blocks_in_chain(non_neg_integer, Block.t) :: list(Block.t)
  # def get_next_blocks_in_chain(number_of_blocks, block) do
  #   Enum.take(find_block_in_chain(block), -number_of_blocks)
  # end
  
  # @spec find_block_in_chain(Block.t) :: list
  # def find_block_in_chain(block) do
  #   tip = get_tip()
  #   find_block_in_chain(block, [tip.prev_block_hash|Block.hash(tip)])
  # end

  # # TODO: should limit this, we don't want to accept really old blocks
  # # anyway, and it will _hammer_ the cache
  # def find_block_in_chain(block, [prev_block_hash|hashes] = acc) do
  #   case get_block_by_hash(prev_block_hash) do
  #     {:ok, prev_block} ->
  # 	if prev_block == block do
  # 	  acc
  # 	else
  # 	  find_block_in_chain(block, [prev_block.prev_block_hash|hashes])
  # 	end
  #     _error ->
  # 	{:error, :notfound}
  #   end
  # end

  # @spec find_first_known_block_hash(list(Block.block_hash)) :: {:ok, Block.t} | {:error, :notfound}
  # def find_first_known_block_hash([]) do
  #   {:error, :notfound}
  # end
  
  # def find_first_known_block_hash([block_hash|block_locator]) do
  #   case LogServer.get_block_by_hash(block_hash) do
  #     {:ok, block} ->
  # 	{:ok, block}
  #     {:error, :notfound} ->
  # 	find_first_known_block_hash(block_locator)
  #   end
  # end
  
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

  def handle_cast({:update_chain_index, old_tip, new_tip}, state = %{chain_index: chain_index, log: log}) do
    if new_tip.header.height > old_tip.header.height do
      case find_first_parent_in_chain(log, chain_index, new_tip) do
	{:ok, block} ->
	  case find_block_range(log, new_tip, block) do
	    {:ok, invalid_hashes} ->
	      {:ok, new_hashes} = find_block_range(log, old_tip, block)
	      for hash <- new_hashes, do: true = :ets.insert_new(chain_index, hash)
	      for hash <- invalid_hashes, do: true = :ets.delete(chain_index, hash)
	    error ->
	      error
	  end
	error ->
	  error
      end
      {:noreply, %{state | tip: new_tip}}
    else
      {:noreply, state}
    end
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

  def find_first_parent_in_chain(log, chain_index, block = %Block{prev_block_hash: prev_block_hash}) do
    case get_block_with_index(log, BlockHashIndex, prev_block_hash) do
      {:ok, prev_block} ->
	case :ets.member(chain_index, prev_block_hash) do
	  false ->
	    find_first_parent_in_chain(log, chain_index, prev_block)
	  true ->
	    {:ok, prev_block}
	end
      error ->
	error
    end
  end

  def find_block_range(log, starting_block, ending_block) do
    find_block_range(log, starting_block, ending_block, [])
  end
  
  def find_block_range(log, starting_block, ending_block, acc) do
    prev_block_hash = starting_block.prev_block_hash
    case get_block_with_index(log, BlockHashIndex, prev_block_hash) do
      {:ok, prev_block} ->
	case prev_block == ending_block do
	  false ->
	    find_block_range(log, prev_block, ending_block, [prev_block_hash|acc])
	  true ->
	    {:ok, [prev_block_hash|acc]}
	end
      error ->
	error
    end
  end

  def get_block_with_index(log, index, block_hash) do
    case index.get_offset(block_hash) do
      {:error, :notfound} ->
	{:error, :notfound}
      {:ok, offsets} when is_list(offsets) ->
	blocks = for offset <- offsets do
	    read_block_with_cache(log, offset) |> Block.decode
	  end
	{:ok, blocks}
      {:ok, offset} ->
	block = read_block_with_cache(log, offset) |> Block.decode
	{:ok, block}
    end
  end

  def read_block_with_cache(log, offset) do
    case Cachex.get(:block_cache, offset) do
      {:error, :no_cache} ->
	{:ok, {encoded_block, _}} = BlockchainLog.read_block(log, offset)
	{:ok, true} = Cachex.set(:block_cache, offset, encoded_block)
	encoded_block
      {:ok, encoded_block} ->
	encoded_block
    end
  end

end

require Logger

alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias WC.Blockchain.PrevBlockHashIndex, as: PrevBlockHashIndex
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Log, as: BlockchainLog
alias WC.Blockchain.InventoryServer, as: InventoryServer
alias WC.Miner.MinerServer, as: MinerServer

defmodule WC.Blockchain.LogServer do
  use GenServer

  @initial_state %{tip: nil, index_complete: false, log: nil, chain_index: nil}

  #
  # PUBLIC
  #
  
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

  @spec update_index(Block.t, Block.t) :: :ok
  def update_index(old_tip, new_tip) do
    GenServer.cast(__MODULE__, {:update_chain_index, old_tip, new_tip})
  end

  @spec get_next_block_hashes_in_chain(non_neg_integer, Block.block_hash) :: list(Block.block_hash)
  def get_next_block_hashes_in_chain(number_of_blocks, starting_block_hash) do
    get_next_block_hashes_in_chain(number_of_blocks, starting_block_hash, [])
  end
  
  def get_next_block_hashes_in_chain(0, _, acc) do
    acc
  end
  
  def get_next_block_hashes_in_chain(number_of_blocks, starting_block_hash, acc) do
    case get_blocks_by_prev_hash(starting_block_hash) do
      {:ok, blocks} ->
	case Enum.filter(blocks, fn block -> is_in_main_chain(Block.hash(block)) end) do
	  [] ->
	    acc
	  [next_block] ->
	    next_block_hash = Block.hash(next_block)
	    get_next_block_hashes_in_chain(number_of_blocks - 1, next_block_hash, [next_block_hash|acc])
	end
      {:error, :notfound} ->
	acc
    end
  end

  @spec is_in_main_chain(Block.block_hash) :: true | false
  def is_in_main_chain(block_hash) do
    GenServer.call(__MODULE__, {:is_in_main_chain, block_hash})
  end

  @spec find_first_block_hash_in_chain(list(Block.block_hash)) :: {:ok, Block.t} | {:error, :notfound}
  def find_first_block_hash_in_chain([]) do
    {:error, :notfound}
  end
  
  def find_first_block_hash_in_chain(block_hashes) do
    case Enum.find(block_hashes, &is_in_main_chain/1) do
      nil ->
	{:error, :notfound}
      block_hash ->
	block_hash
    end
  end
  
  def get_prev_blocks(number_of_blocks) do
    {:ok, tip} = get_tip()
    get_prev_blocks(number_of_blocks, tip)
  end
  
  def get_prev_blocks(number_of_blocks, tip) do
    if tip.header.height == 0 do
      []
    else
      get_prev_blocks(number_of_blocks, tip, [])
    end
  end
  
  def get_prev_blocks(number_of_blocks, tip, acc) do
    {:ok, block} = get_block_by_hash(tip.header.prev_block_hash)
    if (block.header.height == 0) or (tip.header.height - block.header.height == number_of_blocks) do
      acc
    else
      get_prev_blocks(number_of_blocks, block, [block|acc])
    end
  end

  @spec index_complete? :: true | false
  def index_complete? do
    GenServer.call(__MODULE__, :index_complete)
  end

  #
  # GENSERVER CALLBACKS
  #

  def handle_call(_, _, state = %{index_complete: false}) do
    {:reply, {:error, :index_incomplete}, state}
  end

  def handle_call({:get_block_with_index, index, block_hash}, _from, state = %{log: log}) do
    {:reply, get_block_with_index(log, index, block_hash), state}
  end
  
  def handle_call(:get_tip, _from, state = %{tip: tip}) do
    {:reply, {:ok, tip}, state}
  end

  def handle_call({:is_in_main_chain, block_hash}, _from, state = %{chain_index: chain_index}) do
    {:reply, is_in_main_chain(chain_index, block_hash), state}
  end

  def handle_call(:index_complete, _from, state = %{index_complete: index_complete}) do
    {:reply, index_complete, state}
  end
  
  def handle_cast({:update, encoded_block}, state = %{tip: tip, log: log, chain_index: chain_index}) do
    block = Block.decode(encoded_block)
    block_hash = Block.hash(encoded_block)
    offset = BlockchainLog.append_block(log, encoded_block)    
    :ok = BlockHashIndex.insert(block_hash, offset)
    :ok = PrevBlockHashIndex.insert(block.header.prev_block_hash, offset)
    new_tip = if block.header.height > tip.header.height do
      {:ok, ^block} = update_chain_index(log, chain_index, tip, block, false)
      :ok = MinerServer.new_block(block)
      block
    else
      tip
    end
    {:noreply, %{state | tip: new_tip}}
  end

  def handle_cast({:update_index, old_tip, new_tip}, state = %{chain_index: chain_index, log: log}) do
    {:ok, tip} = update_chain_index(log, chain_index, old_tip, new_tip)
    {:noreply, %{state | tip: tip}}
  end
  
  def handle_info({:index_complete, tip}, state) do
    Logger.info "Indexing complete, tip = #{inspect(tip)}"
    # This might happen automagically
    :ok = InventoryServer.getblocks()
    :ok = MinerServer.new_block(tip)
    {:noreply, %{state | tip: tip, index_complete: true}}
  end

  #
  # PRIVATE
  #

  def is_in_main_chain(chain_index, block_hash) do
    :ets.member(chain_index, block_hash)
  end
  
  def find_first_parent_in_chain(log, chain_index, block) do
    prev_block_hash = block.header.prev_block_hash    
    case get_block_with_index(log, BlockHashIndex, prev_block_hash) do
      {:ok, prev_block} ->
	case is_in_main_chain(chain_index, prev_block_hash) do
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
    prev_block_hash = starting_block.header.prev_block_hash
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
      {:missing, nil} ->
	{:ok, {encoded_block, _}} = BlockchainLog.read_block(log, offset)
	{:ok, true} = Cachex.set(:block_cache, offset, encoded_block)
	encoded_block
      {:ok, encoded_block} ->
	encoded_block
    end
  end

  def index_blocks(pid) do
    fn ->
      # Can't share file handles across processes
      {:ok, log} = BlockchainLog.init
      Logger.info "Indexing blocks..."
      tip = index_blocks(log, 0, WC.genesis_block)
      send pid, {:index_complete, tip}
    end
  end
  
  def index_blocks(log, offset, tip) do
    case BlockchainLog.read_block(log, offset) do
      {:ok, {encoded_block, next_offset}} ->
	block_hash = Block.hash(encoded_block)
	Logger.info "Indexing... #{Base.encode16(block_hash)}"
	:ok = BlockHashIndex.insert(block_hash, offset)
	block = Block.decode(encoded_block)
	new_tip = if block.header.height > tip.header.height do
	  # TODO: fix redundant conditionals around update_index
	  :ok = update_index(tip, block)
	  block
	else
	  tip
	end
	index_blocks(log, next_offset, new_tip)
      {:error, :eof} ->
	tip
    end
  end

  @doc "Handle chain reorgs"
  def update_chain_index(log, chain_index, old_tip, new_tip) do
    update_chain_index(log, chain_index, old_tip, new_tip, true)
  end
  
  def update_chain_index(log, chain_index, old_tip, new_tip, getblocks) do
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

      # TODO: maybe eventually use pub/sub to let other procs
      # listen for reorgs and do stuff like getblocks
      # i.e. every connection process could listen,
      # as well as MinerServer
      if getblocks do
	:ok = InventoryServer.getblocks()
      end
      
      {:ok, new_tip}
    else
      {:ok, old_tip}
    end
  end
  
end

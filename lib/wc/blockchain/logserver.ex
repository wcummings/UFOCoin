require Logger

alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias WC.Blockchain.PrevBlockHashIndex, as: PrevBlockHashIndex
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Log, as: BlockchainLog
alias WC.Blockchain.InventoryServer, as: InventoryServer
alias WC.Blockchain.ChainState, as: ChainState
alias WC.Miner.MinerServer, as: MinerServer

defmodule WC.Blockchain.LogServer do
  use GenServer

  @initial_state %{tip: nil, index_complete: false, log: nil}

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
	case Enum.filter(blocks, fn block -> is_in_longest_chain(Block.hash(block)) end) do
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

  @spec find_first_block_hash_in_longest_chain(list(Block.block_hash)) :: {:ok, Block.t} | {:error, :notfound}
  def find_first_block_hash_in_longest_chain(block_hash) do
    GenServer.call(__MODULE__, {:find_first_block_hash_in_longest_chain, block_hash})
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

  def handle_call({:is_in_longest_chain, block_hash}, _from, state) do
    {:reply, is_in_longest_chain(block_hash), state}
  end

  def handle_call(:index_complete, _from, state = %{index_complete: index_complete}) do
    {:reply, index_complete, state}
  end

  def handle_call({:find_first_block_hash_in_longest_chain, block_hashes}, _from, state) do
    case Enum.find(block_hashes, &is_in_longest_chain/1) do
      nil ->
	{:reply, {:error, :notfound}, state}
      block_hash ->
	{:reply, {:ok, block_hash}, state}
    end
  end
  
  def handle_cast({:update, encoded_block}, state = %{tip: tip, log: log}) do
    block = Block.decode(encoded_block)
    block_hash = Block.hash(encoded_block)
    offset = BlockchainLog.append_block(log, encoded_block)    
    :ok = BlockHashIndex.insert(block_hash, offset)
    :ok = PrevBlockHashIndex.insert(block.header.prev_block_hash, offset)

    {:ok, new_tip} = update_chain_state(log, tip, block)
    if block == new_tip do
      # TODO: eventually use pub/sub for reorgs
      :ok = MinerServer.new_block(block)
      :ok = InventoryServer.getblocks()
    end

    {:noreply, %{state | tip: new_tip}}
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

  def is_in_longest_chain(block_hash) do
    case ChainState.get_height_and_cum_difficulty(block_hash) do
      {:error, :notfound} ->
	false
      {:ok, {_, _, in_longest}} ->
	in_longest
    end
  end
  
  def find_first_parent_in_longest_chain(log, block) do
    prev_block_hash = block.header.prev_block_hash    
    case get_block_with_index(log, BlockHashIndex, prev_block_hash) do
      {:ok, prev_block} ->
	case is_in_longest_chain(prev_block_hash) do
	  false ->
	    find_first_parent_in_longest_chain(log, prev_block)
	  true ->
	    {:ok, prev_block}
	end
      error ->
	error
    end
  end

  @doc "Return a range of blocks inclusively, starting_block is the tip."
  @spec find_block_range(BlockchainLog.t, Block.t, Block.t) :: list(BlockHeader.block_hash)
  def find_block_range(log, starting_block, ending_block) do
    find_block_range(log, starting_block, ending_block, [])
  end
  
  def find_block_range(log, starting_block, ending_block, acc) do
    if Block.equal?(starting_block, ending_block) do
      {:ok, [Block.hash(starting_block)|acc]}
    else
      prev_block_hash = starting_block.header.prev_block_hash
      case get_block_with_index(log, BlockHashIndex, prev_block_hash) do
	{:ok, prev_block} ->
	  find_block_range(log, prev_block, ending_block, [prev_block_hash|acc])
	{:error, :notfound} ->
	  {:error, {:notfound, prev_block_hash}}
      end
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
	# Genesis block has invalid prev hash, do not index it normally
	if block.header.height == 0 do
	  :ok = ChainState.insert(block_hash, block.header.height, block.header.difficulty, true)
	  index_blocks(log, next_offset, block)
	else
	  :ok = PrevBlockHashIndex.insert(block.header.prev_block_hash, offset)	
	  {:ok, new_tip} = update_chain_state(log, tip, block)
	  index_blocks(log, next_offset, new_tip)
	end
      {:error, :eof} ->
	tip
    end
  end

  def update_chain_state(log, old_tip, new_tip) do
    {:atomic, result} = :mnesia.transaction(fn ->
      {:ok, {_, cum_difficulty, _}} = ChainState.get_height_and_cum_difficulty(new_tip.header.prev_block_hash)
      new_cum_difficulty = cum_difficulty + new_tip.header.difficulty
      {:ok, {_, old_cum_difficulty, _}} = ChainState.get_height_and_cum_difficulty(Block.hash(old_tip))
      is_longest = new_cum_difficulty > old_cum_difficulty
      :ok = ChainState.insert(Block.hash(new_tip), new_tip.header.height, new_cum_difficulty, is_longest)
      if is_longest do
	case find_first_parent_in_longest_chain(log, new_tip) do
	  {:ok, block} ->
	    {:ok, [_|new_hashes]} = find_block_range(log, new_tip, block)
	    {:ok, [_|invalid_hashes]} = find_block_range(log, old_tip, block)
	    Logger.info "Chain updated:"
	    Logger.info "Added hashes: #{inspect(Enum.map(new_hashes, &Base.encode16/1))}"
	    Logger.info "Removed hashes: #{inspect(Enum.map(invalid_hashes, &Base.encode16/1))}"
	    Logger.info "-------------------"
	    for hash <- invalid_hashes, do: :ok = ChainState.update_longest(hash, false)
	    for hash <- new_hashes, do: :ok = ChainState.update_longest(hash, true)
	    {:ok, new_tip}
	  error ->
	    error
	end
      else
	{:ok, old_tip}
      end
    end)
    result
  end

end

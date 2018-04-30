require Logger

alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex
alias WC.Blockchain.PrevBlockHashIndex, as: PrevBlockHashIndex
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Log, as: BlockchainLog
alias WC.Blockchain.ChainState, as: ChainState
alias WC.Blockchain.OrphanBlockTable, as: OrphanBlockTable
alias WC.Blockchain.InvItem, as: InvItem
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.TxHashIndex, as: TxHashIndex
alias WC.Blockchain.UTXODb, as: UTXODb
alias WC.Miner.MinerServer, as: MinerServer
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.ConnectionRegistry, as: P2PConnectionRegistry

defmodule WC.Blockchain.LogServer do
  use GenServer

  @initial_state %{tip: nil,
		   index_complete: false,
		   log: nil}

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
      BlockchainLog.append_block(log, Block.encode(Block.genesis_block))
    end
    # Setup ets tables for indexes
    :ok = BlockHashIndex.init
    :ok = PrevBlockHashIndex.init
    :ok = TxHashIndex.init
    :ok = ChainState.init
    # :ok = UTXOSet.init
    # Kick off indexer process
    spawn_link index_blocks(self())
    {:ok, %{@initial_state | log: log}}
  end

  @spec get_block_by_hash(BlockHeader.block_hash) :: {:ok, Block.t} | {:error, :notfound}
  def get_block_by_hash(block_hash) do
    GenServer.call(__MODULE__, {:get_block_with_index, BlockHashIndex, block_hash})
  end

  @spec get_block_by_hash!(BlockHeader.block_hash) :: Block.t
  def get_block_by_hash!(block_hash) do
    {:ok, block} = get_block_by_hash(block_hash)
    block
  end

  @spec get_blocks_by_prev_hash(BlockHeader.block_hash) :: {:ok, list(Block.t)} | {:error, :notfound}
  def get_blocks_by_prev_hash(prev_block_hash) do
    GenServer.call(__MODULE__, {:get_block_with_index, PrevBlockHashIndex, prev_block_hash})
  end

  @spec get_tx(TX.tx_hash) :: {:ok, TX.t} | {:error, term}
  def get_tx(tx_hash) do
    case GenServer.call(__MODULE__, {:get_block_with_index, TxHashIndex, tx_hash}) do
      {:error, error} ->
	{:error, error}
      {:ok, block} ->
	[tx] = Enum.filter(block.txs, fn tx -> TX.hash(tx) == tx_hash end)
	{:ok, tx}
    end
  end

  @spec get_tip() :: {:ok, Block.t}
  def get_tip do
    GenServer.call(__MODULE__, :get_tip)
  end

  @spec update(Block.t) :: :ok
  def update(block) do
    GenServer.cast(__MODULE__, {:update, block})
  end

  @spec exists?(BlockHeader.block_hash) :: true | false
  def exists?(block_hash) do
    GenServer.call(__MODULE__, {:exists, block_hash})
  end
  
  @spec find_next_block_hashes_in_chain(non_neg_integer, BlockHeader.block_hash) :: list(BlockHeader.block_hash)
  def find_next_block_hashes_in_chain(number_of_blocks, starting_block_hash) do
    GenServer.call(__MODULE__, {:find_next_block_hashes_in_chain, number_of_blocks, starting_block_hash})
  end

  @spec find_first_block_hash_in_longest_chain(list(BlockHeader.block_hash)) :: {:ok, BlockHeader.block_hash} | {:error, any}
  def find_first_block_hash_in_longest_chain(block_hash) do
    GenServer.call(__MODULE__, {:find_first_block_hash_in_longest_chain, block_hash})
  end

  @spec find_prev_blocks(non_neg_integer) :: list(Block.t)
  def find_prev_blocks(number_of_blocks) do
    GenServer.call(__MODULE__, {:find_prev_blocks, number_of_blocks})
  end

  @spec find_prev_blocks(non_neg_integer, Block.t) :: list(Block.t)
  def find_prev_blocks(number_of_blocks, tip) do
    GenServer.call(__MODULE__, {:find_prev_blocks, number_of_blocks, tip})
  end
  
  @spec index_complete? :: true | false
  def index_complete? do
    GenServer.call(__MODULE__, :index_complete)
  end

  @doc "Build a list of block hashes from newest to genesis, dense to start, then sparse"
  @spec make_block_locator() :: list(BlockHeader.block_hash)
  def make_block_locator do
    GenServer.call(__MODULE__, :make_block_locator)
  end

  @spec make_block_locator(Block.t) :: list(BlockHeader.block_hash)
  def make_block_locator(tip) do
    GenServer.call(__MODULE__, {:make_block_locator, tip})
  end

  #
  # GENSERVER CALLBACKS
  #

  def handle_call(:index_complete, _from, state = %{index_complete: index_complete}) do
    {:reply, index_complete, state}
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

  def handle_call({:exists, block_hash}, _from, state) do
    case BlockHashIndex.get_offset(block_hash) do
      {:ok, _} ->
	case OrphanBlockTable.get(block_hash) do
	  {:ok, _} ->
	    {:reply, true, state}
	  {:error, :notfound} ->
	    {:reply, false, state}
	end
      {:error, :notfound} ->
	{:reply, false, state}
    end
  end
  
  def handle_call({:is_in_longest_chain, block_hash}, _from, state) do
    {:reply, is_in_longest_chain(block_hash), state}
  end

  def handle_call({:find_first_block_hash_in_longest_chain, block_hashes}, _from, state) do
    case Enum.find(block_hashes, &is_in_longest_chain/1) do
      nil ->
	{:reply, {:error, :notfound}, state}
      block_hash ->
	{:reply, {:ok, block_hash}, state}
    end
  end

  def handle_call({:find_next_block_hashes_in_chain, number_of_blocks, starting_block_hash}, _from, state = %{log: log}) do
    {:reply, find_next_block_hashes_in_chain(log, number_of_blocks, starting_block_hash), state}
  end

  def handle_call({:find_prev_blocks, number_of_blocks}, _from, state = %{log: log, tip: tip}) do
    {:reply, find_prev_blocks(log, number_of_blocks, tip), state}
  end

  def handle_call({:find_prev_blocks, number_of_blocks, tip}, _from, state = %{log: log}) do
    {:reply, find_prev_blocks(log, number_of_blocks, tip), state}
  end

  def handle_call(:make_block_locator, _from, state = %{log: log, tip: tip}) do
    {:reply, make_block_locator(log, tip), state}
  end
  
  def handle_call({:make_block_locator, tip}, _from, state = %{log: log}) do
    {:reply, make_block_locator(log, tip), state}
  end

  def handle_call({:get_tx, tx_hash}, _from, state = %{log: log}) do
    {:reply, get_tx(log, tx_hash), state}
  end

  def handle_cast({:update, block}, state = %{tip: tip, log: log}) do
    block_hash = Block.hash(block)
    case get_block_with_index(log, BlockHashIndex, block_hash) do
      {:error, :notfound} ->
	case validate_preappend(log, block) do
	  :ok ->
	    Logger.info "Accepted block: #{BlockHeader.pprint(block.header)}"
	    new_tip = update(log, block, tip)
	    case OrphanBlockTable.get_by_prev_block_hash(block_hash) do
	      {:ok, blocks} ->
		Enum.each(blocks, &update/1)
		{:noreply, %{state | tip: new_tip}}
	      {:error, :notfound} ->
		{:noreply, %{state | tip: new_tip}}
	    end
	  {:error, :orphan} ->
	    # Try to find the orphan's parents! ;_;
	    :ok = OrphanBlockTable.insert(block)
	    :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :getblocks, extra_data: make_block_locator(log, tip)})
	    {:noreply, state}
	  {:error, error} ->
	    Logger.warn "Block rejected: #{inspect(error)}"
	    {:noreply, state}
	end
      {:ok, _} ->
	{:noreply, state}
    end
  end

  def handle_info({:index_complete, tip}, state = %{log: log}) do
    Logger.info "Indexing complete: [tip: #{BlockHeader.pprint(tip.header)}]"
    :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :getblocks, extra_data: make_block_locator(log, tip)})
    :ok = MinerServer.new_block(tip)
    {:noreply, %{state | tip: tip, index_complete: true}}
  end

  #
  # PRIVATE
  #

  def prepare_validate_deps(log, block) do
    case get_block_with_index(log, BlockHashIndex, block.header.prev_block_hash) do
      {:ok, prev_block} ->
	difficulty = Block.get_difficulty(find_prev_blocks(log, Block.difficulty_number_of_blocks(), block))
	{:ok, prev_block, difficulty}
      {:error, :notfound} ->
	{:error, :orphan}
    end
  end
  
  def validate_preappend(log, block) do
    case prepare_validate_deps(log, block) do
      {:ok, prev_block, difficulty} ->
	Block.validate_without_utxodb(prev_block, difficulty, block)
      error ->
	error
    end
  end
  
  @spec update(BlockchainLog.t, Block.t, Block.t) :: :ok
  def update(log, block, tip) do
    encoded_block = Block.encode(block)
    block_hash = Block.hash(block)
    offset = BlockchainLog.append_block(log, encoded_block)
    :ok = BlockHashIndex.insert(block_hash, offset)
    :ok = PrevBlockHashIndex.insert(block.header.prev_block_hash, offset)
    :ok = OrphanBlockTable.delete(block_hash) # Remove orphan entry, if one existed
    
    Enum.each(block.txs, fn tx -> :ok = TxHashIndex.insert(TX.hash(tx), block_hash) end)

    {:ok, new_tip} = update_chain_state(log, tip, block)
    if block == new_tip do
      :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :getblocks, extra_data: make_block_locator(log, block)})
      :ok = MinerServer.new_block(block)
    end

    Logger.info "Advertising block: #{BlockHeader.pprint(block.header)}"
    :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :inv, extra_data: [InvItem.from_block_hash(Block.hash(block))]})
    new_tip
  end
  
  @spec get_tx(BlockchainLog.t, TX.tx_hash) :: {:ok, TX.t} | {:error, :notfound}
  def get_tx(log, tx_hash) do
    case get_block_with_index(log, TxHashIndex, tx_hash) do
      {:error, error} ->
	{:error, error}
      {:ok, block} ->
	[tx] = Enum.filter(block.txs, fn tx -> TX.hash(tx) == tx_hash end)
	{:ok, tx}
    end
  end

  @spec get_tx!(BlockchainLog.t, TX.tx_hash) :: TX.t
  def get_tx!(log, tx_hash) do
    {:ok, tx} = get_tx(log, tx_hash)
    tx
  end
  
  @spec make_block_locator(BlockchainLog.t, Block.t) :: list(BlockHeader.block_hash)
  def make_block_locator(log, tip) do
    genesis_block_hash = Block.hash(Block.genesis_block)
    dense_hashes = for block <- find_prev_blocks(log, 10, tip), do: Block.hash(block)
    if tip.header.height < 10 do
      dense_hashes ++ [genesis_block_hash]
    else
      Enum.uniq(dense_hashes ++ make_prev_block_hashes_sparse(log, tip) ++ [genesis_block_hash]) # HACK: get rid of uniq do it right ffs
    end
  end

  def make_prev_block_hashes_sparse(log, tip) do
    make_prev_block_hashes_sparse(log, tip, 1, 0, [])
  end
  
  def make_prev_block_hashes_sparse(log, tip, step, count, acc) do
    {:ok, block} = get_block_with_index(log, BlockHashIndex, tip.header.prev_block_hash)
    if block.header.height == 0 do
      Enum.reverse(acc)
    else
      if count == step do
	make_prev_block_hashes_sparse(log, block, step * 2, 0, [Block.hash(block)|acc])
      else
	make_prev_block_hashes_sparse(log, block, step, count + 1, acc)
      end
    end
  end

  @spec find_prev_blocks(BlockchainLog.t, non_neg_integer, Block.t) :: list(Block.t)
  def find_prev_blocks(log, number_of_blocks, tip) do
    if tip.header.height == 0 do
      []
    else
      find_prev_blocks(log, number_of_blocks, tip, [])
    end
  end
  
  def find_prev_blocks(log, number_of_blocks, tip, acc) do
    {:ok, block} = get_block_with_index(log, BlockHashIndex, tip.header.prev_block_hash)
    if (block.header.height == 0) or (tip.header.height - block.header.height == number_of_blocks) do
      Enum.reverse(acc)
    else
      find_prev_blocks(log, number_of_blocks, block, [block|acc])
    end
  end
  
  @spec find_next_block_hashes_in_chain(BlockchainLog.t, non_neg_integer, BlockHeader.block_hash) :: list(BlockHeader.block_hash)
  def find_next_block_hashes_in_chain(log, number_of_blocks, starting_block_hash) do
    Enum.reverse(find_next_block_hashes_in_chain(log, number_of_blocks, starting_block_hash, []))
  end
  
  def find_next_block_hashes_in_chain(_, 0, _, acc) do
    acc
  end
  
  def find_next_block_hashes_in_chain(log, number_of_blocks, starting_block_hash, acc) do
    case get_block_with_index(log, PrevBlockHashIndex, starting_block_hash) do
      {:ok, blocks} ->
	case Enum.filter(blocks, fn block -> is_in_longest_chain(Block.hash(block)) end) do
	  [] ->
	    acc
	  [next_block] ->
	    next_block_hash = Block.hash(next_block)
	    find_next_block_hashes_in_chain(log, number_of_blocks - 1, next_block_hash, [next_block_hash|acc])
	end
      {:error, :notfound} ->
	acc
    end
  end

  @spec is_in_longest_chain(BlockHeader.block_hash) :: true | false
  def is_in_longest_chain(block_hash) do
    case ChainState.get_height_and_cum_difficulty(block_hash) do
      {:error, :notfound} ->
	false
      {:ok, {_, _, in_longest}} ->
	in_longest
    end
  end

  @spec find_first_parent_in_longest_chain(BlockchainLog.t, Block.t) :: {:ok, Block.t} | {:error, :notfound}
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

  @spec find_block_range(BlockchainLog.t, Block.t, Block.t) :: {:ok, list(BlockHeader.block_hash)} | {:error, {:notfound, BlockHeader.block_hash}}
  def find_block_range(log, starting_block, ending_block) do
    find_block_range(log, starting_block, ending_block, [Block.hash(starting_block)])
  end
  
  def find_block_range(log, starting_block, ending_block, acc) do
    if Block.equal?(starting_block, ending_block) do
      {:ok, acc}
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

  def get_block_with_index!(log, index, block_hash) do
    {:ok, block} = get_block_with_index(log, index, block_hash)
    block
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
      # Can't share (raw) file handles across processes
      {:ok, log} = BlockchainLog.init
      Logger.info "Indexing blocks..."
      tip = index_blocks(log, 0, Block.genesis_block)
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
    # Logger.info "update_chain_state: old_tip=#{inspect(old_tip)}, new_tip=#{inspect(new_tip)}"
    {:ok, {_, cum_difficulty, _}} = ChainState.get_height_and_cum_difficulty(new_tip.header.prev_block_hash)
    new_cum_difficulty = cum_difficulty + new_tip.header.difficulty
    {:ok, {_, old_cum_difficulty, _}} = ChainState.get_height_and_cum_difficulty(Block.hash(old_tip))
    is_longest = new_cum_difficulty > old_cum_difficulty
    :ok = ChainState.insert(Block.hash(new_tip), new_tip.header.height, new_cum_difficulty, false)
    if is_longest do
      case find_first_parent_in_longest_chain(log, new_tip) do
	{:ok, parent} ->
	  # 1. Update longest chain
	  {:ok, [_|added_block_hashes]} = find_block_range(log, new_tip, parent)
	  {:ok, [_|removed_block_hashes]} = find_block_range(log, old_tip, parent)
	  if length(removed_block_hashes) > 0 do
	    Logger.info "-------------------"	      
	    Logger.info "Chain re-organized:"
	    Logger.info "Common parent: #{Block.hash(parent) |> Base.encode16}"
	    Logger.info "Added: #{inspect(Enum.map(added_block_hashes, &Base.encode16/1))}"
	    Logger.info "Removed: #{inspect(Enum.map(removed_block_hashes, &Base.encode16/1))}"
	    Logger.info "-------------------"
	  end

	  removed_blocks = Enum.map(removed_block_hashes, fn block_hash -> get_block_with_index!(log, BlockHashIndex, block_hash) end)
	  added_blocks = Enum.map(added_block_hashes, fn block_hash -> get_block_with_index!(log, BlockHashIndex, block_hash) end)
	  
	  case update_utxodb(log, removed_blocks, added_blocks) do
	    :ok ->
	      :ok = ChainState.update_longest(Block.hash(new_tip), true)
	      # Enum.each(added_blocks, fn block -> Logger.info "Block accepted: #{BlockHeader.pprint(block.header)}" end)
	      for hash <- removed_block_hashes, do: :ok = ChainState.update_longest(hash, false)
	      for hash <- added_block_hashes, do: :ok = ChainState.update_longest(hash, true)
	      {:ok, new_tip}
	    {:error, error} ->
	      Logger.warn "Block rejected: #{inspect(error)}"
	      {:ok, old_tip}
	  end
	error ->
	  error
      end
    else
      {:ok, old_tip}
    end
  end

  def update_utxodb(log, removed_blocks, added_blocks) do
    # First rollback blocks back to the shared parent
    db = UTXODb.new(:inmem)
    utxo_ops = Enum.flat_map(removed_blocks, fn block -> UTXODb.undo_block_changeset(db, block) end)
    db = Enum.reduce(utxo_ops, db, fn (op, db) -> UTXODb.apply_op(db, op) end)
    # Validate and add each new block
    case verify_blocks_and_update_utxodb(log, db, added_blocks) do
      {:ok, new_db} ->
	# Commit our in memory changes to mnesia now that we know all the blocks are valid
	UTXODb.commit(new_db)
	:ok
      error ->
	# Log the error and do nothing
	Logger.warn "Block rejected during reorg, aborting: #{inspect(error)}"
	error
    end
  end

  def verify_blocks_and_update_utxodb(log, db, [block|rest]) do
    {:ok, prev_block, difficulty} = prepare_validate_deps(log, block)
    case Block.validate(db, prev_block, difficulty, block) do
      :ok ->
	utxo_ops = UTXODb.block_changeset(block)
	db = Enum.reduce(utxo_ops, db, fn (op, db) -> UTXODb.apply_op(db, op) end)
	verify_blocks_and_update_utxodb(log, db, rest)
      error ->
	error
    end
  end

  def verify_blocks_and_update_utxodb(_, db, []), do: {:ok, db}
  
end

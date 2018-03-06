require Logger

alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.OrphanBlockTable, as: OrphanBlockTable
alias WC.Blockchain.InventoryServer, as: InventoryServer

defmodule WC.Blockchain.BlockValidatorServer do
  @moduledoc """
  Serialize block validation, so we never validate the same block twice
  """
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @spec validate_block(Block.t) :: :ok | {:error, Block.block_validation_error}
  def validate_block(block) do
    GenServer.call(__MODULE__, {:validate_block, block})
  end

  def init([]) do
    {:ok, %{}}
  end

  def handle_call({:validate_block, block = %Block{}}, _from, state) do
    block_hash = Block.hash(block)
    result = LogServer.get_block_by_hash(block_hash)
    case result do
      {:ok, _block} ->
	{:reply, {:error, :alreadyaccepted}, state}
      {:error, :notfound} ->
	case Block.validate(block) do
	  :ok ->
	    LogServer.update(block)
	    orphans = find_orphans(block)
	    if length(orphans) > 0 do
	      orphan_hashes = Enum.map(orphans, &Block.hash/1) |> Enum.map(&Base.encode16/1)
	      Logger.info "Found orphans for #{Base.encode16(block_hash)}: #{inspect(orphan_hashes)}"
	    end
	    # Nested mnesia transactions
	    :mnesia.transaction(fn ->
	      Enum.each(orphans, fn _ -> :ok = LogServer.update(block) end) # NOTE: This is order sensitive
	      Enum.map(orphans, &Block.hash/1)
	      |> Enum.each(&OrphanBlockTable.delete/1)
	    end)
	    {:reply, :ok, state}
	  {:error, :orphan} ->
	    # See Block.validate/1 in block.ex for details.
	    :ok = OrphanBlockTable.insert(block)
	    :ok = InventoryServer.getblocks()
	    {:reply, {:error, :orphan}, state}
	  {:error, error} ->
	    {:reply, {:error, error}, state}
	end
    end
  end
  
  def find_orphans(block) do
    find_orphans(block, [])
  end
  
  def find_orphans(block, orphans) do
    case OrphanBlockTable.get_by_prev_block_hash(Block.hash(block)) do
      {:ok, parent_block} ->
	# This would be a nice place to recurse to validate_block/1,
	# if it weren't a genserver call
	find_orphans(parent_block, [parent_block|orphans])
      {:error, :notfound} ->
	orphans
    end
  end
  
end

require Logger

alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.OrphanBlockTable, as: OrphanBlockTable
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.ConnectionRegistry, as: P2PConnectionRegistry

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
    GenServer.cast(__MODULE__, {:validate_block, block})
  end

  def init([]) do
    {:ok, %{}}
  end

  def handle_cast({:validate_block, block = %Block{}}, state) do
    block_hash = Block.hash(block)
    result = LogServer.get_block_by_hash(block_hash)
    case result do
      {:ok, _block} ->
	{:noreply, state}	
      {:error, :notfound} ->
	case Block.validate(block) do
	  :ok ->
	    Logger.info "Block accepted: #{BlockHeader.pprint(block.header)}"	    
	    :ok = LogServer.update(block)
	    case OrphanBlockTable.get_by_prev_block_hash(block_hash) do
	      {:ok, blocks} ->
		Logger.info "Found orphan child blocks: #{inspect(Enum.map(blocks, &Block.hash/1) |> Enum.map(&Base.encode16/1))}"
		Enum.each(blocks, &validate_block/1)
		{:noreply, state}		
	      {:error, :notfound} ->
		{:noreply, state}
	    end
 	  {:error, :orphan} ->
	    # See Block.validate/1 in block.ex for details.
	    :ok = OrphanBlockTable.insert(block)
	    :ok = send_getblocks()
	    {:noreply, state}
	  {:error, error} ->
	    Logger.warn "Block rejected, reason: #{inspect(error)}, #{BlockHeader.pprint(block.header)}"	    
	    {:noreply, state}
	end
    end
  end

  def send_getblocks do
    # Check that we don't spam the network during the initial indexing    
    # if LogServer.index_complete?() do
    :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :getblocks, extra_data: LogServer.get_block_locator()})
    # end
  end
  
end

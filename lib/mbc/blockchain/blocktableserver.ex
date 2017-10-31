require Logger

alias MBC.Blockchain.BlockTable, as: BlockTable
alias MBC.Blockchain.Block, as: Block

defmodule MBC.Blockchain.BlockTableServer do
  use GenServer

  @initial_state %{}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end
  
  def init([]) do
    {:ok, %{}}
  end

  def handle_cast({:new_block, block = %Block{prev_block_hash = prev_block_hash}}, state) do
    case BlockTable.get(prev_block_hash) do
      :undefined ->
	Logger.warn("Received orphan block with hash = #{prev_block_hash}")
	# FIXME:
	# Do we need a separate table for orphans?
	# Any reason they can't just be disconnected entries w/ height = nil?
	:ok = BlockTable.insert(block)
      %Block{height = height} ->
	:ok = BlockTable.insert(%{block | height: height})
    end
    {:noreply, state}
  end

  def check_nonce(block) do
    if Block.check_nonce(block) do
      check_prev_block_hash(block)
    else
      {:error, :badnonce}
    end
  end

  def follow_block_to_genesis(block = %Block{prev_block_hash}) do
    genesis_block = OTC.genesis_block    
    case BlockTable.get(prev_block_hash) do
      genesis_block ->
	:true
      prev_block = %Block{} ->
	follow_block_to_genesis(prev_block)
      :undefined ->
	:false
    end
  end
  
end

# Serialize block validation, so we never validate the same block twice

alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockHeader, as: BlockHeader
alias MBC.Blockchain.LogServer, as: LogServer

defmodule MBC.Blockchain.BlockValidatorServer do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def validate_block(block) do
    GenServer.call(__MODULE__, {:validate_block, block})
  end

  def init([]) do
    {:ok, %{}}
  end

  def handle_call({:validate_block, block = %Block{}}, _from, state) do
    case LogServer.get_block_by_hash(BlockHeader.hash(block.header)) do
      {:ok, _block} ->
	{:reply, {:error, :alreadyaccepted}, state}
      {:error, :notfound} ->
	result = Block.validate(block)
	if result == :ok do
	  LogServer.update(block)
	end
	{:reply, result, state}
    end
  end
  
end

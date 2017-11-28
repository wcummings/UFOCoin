# Serialize block validation, so we never validate the same block twice

alias MBC.Blockchain.Block, as: Block
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
    case LogServer.get_block_by_hash(Block.hash(block)) do
      {:ok, _block} ->
	{:reply, {:error, :alreadyaccepted}, state}
      {:error, :notfound} ->
	{:reply, Block.validate(block), state}
    end
  end
  
end

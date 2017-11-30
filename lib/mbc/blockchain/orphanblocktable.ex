alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockHeader, as: BlockHeader

defmodule MBC.Blockchain.OrphanBlockTable do

  def init do
    :mnesia.create_table(OrphanBlockTable, [attributes: [:block_hash, :block]])
  end

  @spec insert(Block.t) :: :ok | {:error, term}
  def insert(block) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({OrphanBlockTable, Block.hash(block), block}) end)
    result
  end

  @spec get(BlockHeader.block_hash) :: {:ok, Block.t} | {:error, :notfound}
  def get(block_hash) do
    case :mnesia.transaction(fn -> :mnesia.read(OrphanBlockTable, block_hash) end) do
      {:atomic, [{OrphanBlockTable, ^block_hash, block}]} ->
	{:ok, block}
      {:atomic, []} ->
	{:error, :notfound}
    end  
  end

  @spec delete(BlockHeader.block_hash) :: :ok
  def delete(block_hash) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.delete({OrphanBlockTable, block_hash}) end)
    :ok
  end
  
end

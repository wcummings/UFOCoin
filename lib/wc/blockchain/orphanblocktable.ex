alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.OrphanBlockTable do

  def init do
    :mnesia.create_table(OrphanBlockTable, [attributes: [:block_hash, :prev_block_hash, :block], index: [:prev_block_hash]])
  end

  @spec insert(Block.t) :: :ok | {:error, term}
  def insert(block = %Block{header: %BlockHeader{prev_block_hash: prev_block_hash}}) do
    {:atomic, result} = :mnesia.transaction(fn ->
      :mnesia.write({OrphanBlockTable, Block.hash(block), prev_block_hash, block})
    end)
    result
  end

  @spec get(BlockHeader.block_hash) :: {:ok, Block.t} | {:error, :notfound}
  def get(block_hash) do
    case :mnesia.transaction(fn -> :mnesia.read(OrphanBlockTable, block_hash) end) do
      {:atomic, [{OrphanBlockTable, ^block_hash, _, block}]} ->
	{:ok, block}
      {:atomic, []} ->
	{:error, :notfound}
    end  
  end

  @spec get_by_prev_block_hash(BlockHeader.block_hash) :: {:ok, Block.t} | {:error, :notfound}
  def get_by_prev_block_hash(prev_block_hash) do
    {:atomic, result} = :mnesia.transaction(fn ->
      :mnesia.index_read(OrphanBlockTable, prev_block_hash, :prev_block_hash)
    end)

    case result do
      [{OrphanBlockTable, _block_hash, _prev_block_hash, block}] ->
	{:ok, block}
      [] ->
	{:error, :notfound}
    end
  end

  @spec delete(BlockHeader.block_hash) :: :ok
  def delete(block_hash) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.delete({OrphanBlockTable, block_hash}) end)
    :ok
  end
  
end

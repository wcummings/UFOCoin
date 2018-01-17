alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.BlockHashIndex do

  def init do
    :mnesia.create_table(BlockHashIndexTable, [attributes: [:block_hash, :offset], type: :set])
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer()) :: term
  def insert(block_hash, offset) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({BlockHashIndexTable, block_hash, offset}) end)
    result
  end

  @spec get_offset(BlockHeader.block_hash) :: {:ok, list(non_neg_integer)} | {:error, :notfound}
  def get_offset(block_hash) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(BlockHashIndexTable, block_hash) end)
    case result do
      [{BlockHashIndexTable, ^block_hash, offset}] ->
	{:ok, offset}
      [] ->
	{:error, :notfound}
    end
  end

end

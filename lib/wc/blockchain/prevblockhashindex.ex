alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.PrevBlockHashIndex do

  def init do
    :mnesia.create_table(PrevBlockHashIndexTable, [attributes: [:prev_block_hash, :offset], type: :bag])
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer()) :: term
  def insert(prev_block_hash, offset) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({PrevBlockHashIndexTable, prev_block_hash, offset}) end)
    result
  end

  @spec get_offset(BlockHeader.block_hash) :: {:ok, non_neg_integer()} | {:error, :notfound}
  def get_offset(prev_block_hash) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(PrevBlockHashIndexTable, prev_block_hash) end)
    case result do
      [] ->
	{:error, :notfound}
      records ->
	{:ok, Enum.map(records, fn {PrevBlockHashIndexTable, ^prev_block_hash, offset} -> offset end)}
    end
  end
  
end

alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.PrevBlockHashIndex do

  def init do
    :mnesia.create_table(PrevBlockHashIndexTable, [attributes: [:prev_block_hash, :offset], type: :set])
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer()) :: term
  def insert(prev_block_hash, offset) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({PrevBlockHashIndexTable, {prev_block_hash, offset}, offset}) end)
    result
  end

  @spec get_offset(BlockHeader.block_hash) :: {:ok, list(non_neg_integer)} | {:error, :notfound}
  def get_offset(prev_block_hash) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.match_object({PrevBlockHashIndexTable, {prev_block_hash, :_}, :_}) end)
    case result do
      [] ->
	{:error, :notfound}
      records ->
	{:ok, Enum.map(records, fn {PrevBlockHashIndexTable, {^prev_block_hash, offset}, offset} -> offset end)}
    end
  end
  
end

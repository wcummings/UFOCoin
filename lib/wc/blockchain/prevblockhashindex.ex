alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.PrevBlockHashIndex do

  def init do
    :prev_block_hash_index = :ets.new(:prev_block_hash_index, [:public, :named_table])
    :ok
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer()) :: term
  def insert(prev_block_hash, offset) do
    :ets.insert(:prev_block_hash_index, {prev_block_hash, offset})
    :ok
  end

  @spec get_offset(BlockHeader.block_hash) :: {:ok, list(non_neg_integer)} | {:error, :notfound}
  def get_offset(prev_block_hash) do
    case :ets.lookup(:prev_block_hash_index, prev_block_hash) do
      [] ->
	{:error, :notfound}
      records ->
	{:ok, Enum.map(records, fn {^prev_block_hash, offset} -> offset end)}
    end
  end
  
end

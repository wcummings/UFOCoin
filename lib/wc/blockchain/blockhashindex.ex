alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.BlockHashIndex do

  @spec init :: :ok
  def init do
    :block_hash_index = :ets.new(:block_hash_index, [:set, :public, :named_table])
    :ok
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer()) :: :ok
  def insert(block_hash, offset) do
    :true = :ets.insert_new(:block_hash_index, {block_hash, offset})
    :ok
  end

  @spec get_offset(BlockHeader.block_hash) :: {:ok, list(non_neg_integer)} | {:error, :notfound}
  def get_offset(block_hash) do
    case :ets.lookup(:block_hash_index, block_hash) do
      [{^block_hash, offset}] ->
	{:ok, offset}
      [] ->
	{:error, :notfound}
    end
  end

end

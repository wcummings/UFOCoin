alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.ChainState do
  @moduledoc """
  Keep track of the "longest" chain.
  """

  def init do
    :chain_state = :ets.new(:chain_state, [:public, :named_table, :set])
    :ok
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer, non_neg_integer, boolean) :: :ok
  def insert(block_hash, height, cum_difficulty, in_longest) do
    :ets.insert(:chain_state, {block_hash, height, cum_difficulty, in_longest})
    :ok
  end
  
  @spec get_height_and_cum_difficulty(BlockHeader.block_hash) :: {:ok, {non_neg_integer, non_neg_integer, boolean}} | {:error, :notfound}
  def get_height_and_cum_difficulty(block_hash) do
    case :ets.lookup(:chain_state, block_hash) do
      [] ->
	{:error, :notfound}
      [{^block_hash, height, cum_difficulty, in_longest}] ->
	{:ok, {height, cum_difficulty, in_longest}}
    end
  end

  @spec update_longest(BlockHeader.block_hash, boolean) :: :ok
  def update_longest(block_hash, in_longest) do
    {:ok, {height, cum_difficulty, _}} = get_height_and_cum_difficulty(block_hash)
    insert(block_hash, height, cum_difficulty, in_longest)
  end
  
end

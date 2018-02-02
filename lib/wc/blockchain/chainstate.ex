alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.ChainState do
  @moduledoc """
  Keep track of the "longest" chain.
  """

  def init do
    :mnesia.create_table(ChainStateTable, [attributes: [:block_hash, :height, :cum_difficulty, :in_longest]])
  end

  @spec insert(BlockHeader.block_hash, non_neg_integer, non_neg_integer, boolean) :: term
  def insert(block_hash, height, cum_difficulty, in_longest) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({ChainStateTable, block_hash, height, cum_difficulty, in_longest}) end)
    result
  end
  
  @spec get_height_and_cum_difficulty(BlockHeader.block_hash) :: {:ok, {non_neg_integer, non_neg_integer}} | {:error, :notfound}
  def get_height_and_cum_difficulty(block_hash) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(ChainStateTable, block_hash) end)
    case result do
      [{ChainStateTable, ^block_hash, height, cum_difficulty, in_longest}] ->
	{:ok, {height, cum_difficulty, in_longest}}
      [] ->
	{:error, :notfound}
    end
  end

  @spec update_longest(BlockHeader.block_hash, boolean) :: :ok
  def update_longest(block_hash, in_longest) do
    {:ok, {height, cum_difficulty, _}} = get_height_and_cum_difficulty(block_hash)
    insert(block_hash, height, cum_difficulty, is_longest)
  end
  
end

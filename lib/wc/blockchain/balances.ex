require Logger

alias WC.Blockchain.Block, as: Block

defmodule WC.Blockchain.Balances do

  @spec init :: :ok
  def init do
    :balances = :ets.new(:balances, [:named_table, :public, :set])
    :ok
  end

  @spec update(list(Block.t), list(Block.t), map()) :: :ok
  def update(removed_blocks, added_blocks, tx_map) do
    :ok = rollback_blocks(removed_blocks, tx_map)
    :ok = insert_blocks(added_blocks)
  end
  
  @spec check_balance(binary()) :: non_neg_integer
  def check_balance(fingerprint) do
    case :ets.lookup(:balances, fingerprint) do
      [{^fingerprint, balance}] ->
	balance
      [] ->
	0
    end
  end

  #
  # PRIVATE
  #

  @spec rollback_blocks(list(Block.t), map()) :: :ok
  def rollback_blocks([block|rest], tx_map) do
    Enum.flat_map(block.txs, fn tx -> tx.inputs end)
    |> Enum.map(fn input -> tx_map[input.tx_hash].outputs[input.offset] end)
    |> Enum.each(fn output -> update_balance(output.fingerprint, -output.value) end)
    rollback_blocks(rest, tx_map)
  end

  def rollback_blocks([], _), do: :ok
  
  @spec insert_blocks(list(Block.t)) :: :ok
  def insert_blocks([block|rest]) do
    Enum.flat_map(block.txs, fn tx -> tx.outputs end)
    |> Enum.each(fn output -> update_balance(output.fingerprint, output.value) end)
    insert_blocks(rest)
  end

  def insert_blocks([]), do: :ok

  @spec update_balance(binary, non_neg_integer) :: non_neg_integer
  def update_balance(fingerprint, value) do
    :ets.update_counter(:balances, fingerprint, {2, value}, {fingerprint, value})
  end
  
end

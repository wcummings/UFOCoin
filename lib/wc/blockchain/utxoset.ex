alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.UTXOSet do

  @spec init :: :ok
  def init do
    :utxo_set = :ets.new(:utxo_set, [:named_set, :public, :set])
    :ok
  end

  @spec update(list(BlockHeader.block_hash), list(BlockHeader.block_hash), Map.t) :: :ok
  def update(removed_blocks, added_blocks, tx_map) do
    :ok = rollback_blocks(removed_blocks, tx_map)
    :ok = insert_blocks(added_blocks)
  end

  @spec get_output(TX.tx_hash, non_neg_integer) :: {:ok, Output.t} | {:error, :notfound}
  def get_output(tx_hash, offset) do
    case :ets.lookup(:utxo_set, {tx_hash, offset}) do
      [] ->
	{:error, :notfound}
      [{{^tx_hash, ^offset}, output}] ->
	{:ok, output}
    end
  end
  
  #
  # PRIVATE
  #

  def rollback_blocks([block|rest], tx_map) do
    # 1. Delete outputs from the block
    Enum.each(utxo_keys(block), fn key -> :ok = :ets.delete(:utxo_set, key) end)
    # 2. Re-add outputs referenced in the block's inputs
    Enum.flat_map(block.txs, fn tx -> get_utxo_values_for_inputs(tx, tx_map) end)
    |> Enum.each(fn value -> :true = :ets.insert(:utxo_set, value) end)
    rollback_blocks(rest, tx_map)
  end

  def rollback_blocks([], _, _) do
    :ok
  end

  def insert_blocks([block|rest]) do
    Enum.each(utxo_keys(block), fn key -> :ok = :ets.delete(:utxo_set, key) end)
    Enum.each(utxo_values(block), fn value -> :true = :ets.insert(:utxo_set, value) end)
    insert_blocks(rest)
  end

  def insert_blocks([], _) do
    :ok
  end

  @spec get_utxo_values_for_inputs(TX.t, Map.t) :: list({{TX.tx_hash, non_neg_integer}, Output.t})
  def get_utxo_values_for_inputs(%TX{inputs: inputs}, tx_map) do
    for input <- inputs do
      tx = tx_map[input.tx_hash]
      {{TX.hash(tx), input.offset}, tx.outputs[input.offset]}
    end
  end
  
  @doc "Returns keys for fetching/deleting outputs based on inputs."
  @spec utxo_keys(Block.t) :: list({TX.tx_hash, non_neg_integer})
  def utxo_keys(%Block{txs: txs}) do
    Enum.flat_map(txs, &utxo_key/1)
  end
  
  @doc "Returns records for creating outputs."
  @spec utxo_values(Block.t) :: list({{TX.tx_hash, non_neg_integer}, Output.t})
  def utxo_values(%Block{txs: txs}) do
    Enum.flat_map(txs, &utxo_value/1)
  end

  def utxo_value(tx = %TX{outputs: outputs}) do
    tx_hash = TX.hash(tx)
    Enum.with_index(outputs)
    |> Enum.map(fn {output, i} -> {{tx_hash, i}, output} end)
  end

  def utxo_key(%TX{inputs: inputs}) do
    Enum.map(inputs, fn input -> {input.tx_hash, input.offset} end)
  end

end

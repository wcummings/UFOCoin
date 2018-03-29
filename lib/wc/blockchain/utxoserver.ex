alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.Output, as: Output

# TODO: cache UTXO db for last N blocks

defmodule WC.Blockchain.UTXOServer do
  @moduledoc """
  Maintain a database of unspent outputs.
  Processes _already validated_ blocks.
  """
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    {:ok, %{utxo_set: :ets.new(:utxo_set, [:set])}}
  end

  @spec update(list(BlockHeader.block_hash), list(BlockHeader.block_hash)) :: ok
  def update(removed_hashes, added_hashes) do
    GenServer.cast(__MODULE__, {:update, removed_hashes, added_hashes})
  end

  @spec get_tx(TX.tx_hash) :: {:ok, TX.t} | {:error, :notfound}
  def get_tx(tx_hash) do
    GenServer.call(__MODULE__, {:get_tx, tx_hash})
  end
  
  def handle_cast({:update, removed_hashes, added_hashes}, state = %{utxo_set: utxo_set}) do
    removed_blocks = Enum.map(removed_hashes, fn block_hash ->
      {:ok, block} = LogServer.get_block_by_hash(block_hash)
      block
    end)
    added_blocks = Enum.map(added_hashes, fn block_hash ->
      {:ok, block} = LogServer.get_block_by_hash(block_hash)
      block
    end)
    :ok = rollback_blocks(removed_blocks)
    :ok = insert_blocks(added_blocks)
    {:noreply, state}
  end

  def handle_call({:get_tx, tx_hash}, _from, state = %{utxo_set: utxo_set}) do
    {:reply, get_tx_from_utxo_set(tx_hash, utxo_set), state}
  end

  #
  # PRIVATE
  #

  def rollback_blocks([block|rest], utxo_set) do
    # 1. Delete outputs from the block
    Enum.each(utxo_keys(block), fn key -> :ok = :ets.delete(utxo_set, key) end)
    # 2. Re-add outputs referenced in the block's inputs
    Enum.flat_map(block.txs, &get_utxo_values_for_inputs/1)
    |> Enum.each(value -> :true = :ets.insert(utxo_set, value) end)
    rollback_blocks(rest, utxo_set)
  end

  def rollback_blocks([], _) do
    :ok
  end

  def insert_blocks([block|rest], utxo_set) do
    Enum.each(utxo_keys(block), fn key -> :ok = :ets.delete(utxo_set, key) end)
    Enum.each(utxo_values(block), fn value -> :true = :ets.insert(utxo_set, value) end)
    insert_blocks(rest, utxo_set)
  end

  def insert_blocks([], _) do
    :ok
  end

  @spec get_utxo_values_for_inputs(TX.t) :: list({{TX.tx_hash, non_neg_integer}, Output.t})
  def get_utxo_values_for_inputs(%TX{inputs: inputs}) do
    for input <- inputs do
      {:ok, tx} = LogSever.get_tx(input.tx_hash)
      {{TX.hash(tx), input.offset}, tx.outputs[input.offset]}
    end
  end
  
  @doc "Returns keys for fetching/deleting outputs based on inputs."
  @spec get_utxo_keys(Block.t) :: list({TX.tx_hash, non_neg_integer})
  def utxo_keys(%Block{tx: txs}) do
    Enum.flat_map(txs, &get_utxo_key/1)
  end
  
  @doc "Returns records for creating outputs."
  @spec get_utxo_recs(Block.t) :: list({{TX.tx_hash, non_neg_integer}, Output.t})
  def utxo_values(%Block{txs: txs}) do
    Enum.flat_map(txs, &get_utxo_value/1)
  end

  def utxo_value(tx = %TX{outputs: outputs}) do
    tx_hash = TX.hash(tx)
    Enum.with_index(outputs)
    |> Enum.map(fn {output, i} -> {{tx_hash, i}, output}} end)
  end

  def utxo_key(tx = %TX{inputs: inputs}) do
    tx_hash = TX.hash(tx)
    Enum.map(inputs, fn input -> {input.tx_hash, input.offset} end)
  end

  def get_tx_from_utxo_set(tx_hash, utxo_set) do
    case :ets.lookup(utxo_set, tx_hash) do
      [] ->
	{:error, :notfound}
      [{^tx_hash, tx}] ->
	{:ok, tx}
    end
  end

end

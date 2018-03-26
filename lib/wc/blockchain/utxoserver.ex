alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.Block, as: Block

# TODO: cache UTXO db for last N blocks

defmodule WC.Blockchain.UTXOServer do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    {:ok, %{utxo_set: :ets.new(:utxo_set, [:set])}}
  end

  @spec update(list(BlockHeader.block_hash), list(BlockHeader.block_hash) :: ok
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
    case :ets.lookup(utxo_set, tx_hash) do
      [] ->
	{:reply, {:error, :notfound}, state}
      [{^tx_hash, tx}] ->
	{:reply, {:ok, tx}, state}
    end
  end

  def rollback_blocks([block|rest], utxo_set) do
    # 1. Delete outputs from the block
    Enum.each(block.txs, fn tx -> :true = :ets.delete(utxo_set, TX.hash(tx)) end)
    # 2. Re-add outputs referenced in the block's inputs
    get_inputs(block)
    |> Enum.each(fn tx_hash ->
      {:ok, tx} = Logserver.get_tx(tx_hash)
      :true = :ets.insert(utxo_set, {tx_hash, tx})
    end)
    rollback_blocks(rest, utxo_set)
  end

  def rollback_blocks([], _) do
    :ok
  end

  def insert_blocks([block|rest], utxo_set) do
    # 1. Delete outputs referenced in the block's inputs
    get_inputs(block)
    |> Enum.each(fn tx_hash -> :true = :ets.delete(utxo_set, tx_hash) end)
    # 2. Add outputs from this block
    Enum.each(block.txs, fn tx -> :true = :ets.insert(utxo_set, {TX.hash(tx), tx}) end)
    insert_blocks(rest, utxo_set)
  end

  def insert_blocks([], _) do
    :ok
  end

  def get_inputs(%Block{txs: txs}) do
    Enum.flat_map(block.txs, fn tx -> tx.inputs end)
    |> Enum.map(fn input -> input.tx_hash end)
  end

end

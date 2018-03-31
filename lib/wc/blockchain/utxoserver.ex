alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.UTXOSet, as: UTXOSet
alias WC.Blockchain.Block, as: Block

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
    {:ok, %{utxo_set: UTXOSet.new()}}
  end

  @spec update(list(BlockHeader.block_hash), list(BlockHeader.block_hash)) :: :ok
  def update(removed_block_hashes, added_block_hashes) do
    GenServer.cast(__MODULE__, {:update, removed_block_hashes, added_block_hashes})
  end

  @spec get_tx(TX.tx_hash) :: {:ok, TX.t} | {:error, :notfound}
  def get_tx(tx_hash) do
    GenServer.call(__MODULE__, {:get_tx, tx_hash})
  end
  
  def handle_cast({:update, removed_block_hashes, added_block_hashes}, state = %{utxo_set: utxo_set}) do
    removed_blocks = Enum.map(removed_block_hashes, &LogServer.get_block_by_hash!/1)
    added_block = Enum.map(added_block_hashes, &LogServer.get_block_by_hash!/1)
    # Fetch all txs referenced in inputs of removed blocks
    tx_map = Enum.flat_map(removed_blocks, fn %Block{txs: txs} -> txs.inputs end)
    |> Enum.map(fn input ->
      {:ok, tx} = LogServer.get_tx(input.tx_hash)
      tx
    end)
    |> Enum.group_by(fn tx -> TX.hash(tx) end)
    |> Enum.map(fn {k, [v]} -> {k, v} end)
    UTXOSet.update(removed_blocks, added_block, tx_map, utxo_set)
    {:noreply, state}
  end

  def handle_call({:get_tx, tx_hash}, _from, state = %{utxo_set: utxo_set}) do
    {:reply, UTXOSet.get_tx_from_utxo_set(tx_hash, utxo_set), state}
  end

end

require Logger

alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.TX, as: TX

defmodule WC.Blockchain.UTXODb do
  @moduledoc """
  Mnesia table for UTXO, and utilities for updating it in memory.
  """

  @opaque t :: :raw | {:inmem, %{}}
  @type db_type :: :raw | :inmem
  @type utxo_key :: {TX.tx_hash, Input.offset}
  @type utxo_record :: {utxo_key, {binary(), non_neg_integer()}}
  @type utxo_op :: {:add, utxo_record} | {:delete, utxo_key}
  
  def init do
    :mnesia.create_table(UTXODbTable, [attributes: [:tx_hash_and_offset, :output], type: :set])
  end

  @spec new(db_type) :: t
  def new(:raw) do
    :raw
  end

  def new(:inmem) do
    {:inmem, %{}}
  end

  @spec apply_op(t, utxo_op) :: t
  def apply_op(:raw, {:add, {utxo_key, output}}) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.write({UTXODbTable, utxo_key, Output.to_tuple(output)}) end)
    :raw
  end

  def apply_op({:inmem, updates}, {:add, {utxo_key, output}}) do
    {:inmem, Map.put(updates, utxo_key, output)}
  end

  def apply_op(:raw, {:delete, utxo_key}) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.delete({UTXODbTable, utxo_key}) end)
    :raw
  end

  def apply_op({:inmem, updates}, {:delete, utxo_key}) do
    # Store undefined to track deletes
    # Maybe cleaner to use two maps here?
    {:inmem, Map.put(updates, utxo_key, :undefined)}
  end

  @spec get(t, TX.tx_hash, non_neg_integer) :: {:ok, Output.t} | {:error, :not_found}
  def get(:raw, tx_hash, offset) do
    case :mnesia.transaction(fn -> :mnesia.read(UTXODbTable, {tx_hash, offset}) end) do
      {:atomic, [{UTXODbTable, {^tx_hash, ^offset}, output}]} ->
	{:ok, Output.from_tuple(output)}
      {:atomic, []} ->
	{:error, :not_found}
    end
  end

  def get({:inmem, updates}, tx_hash, offset) do
    case Map.get(updates, {tx_hash, offset}) do
      nil ->
	get(:raw, tx_hash, offset)
      :undefined ->
	{:error, :not_found}
      output ->
	{:ok, output}
    end
  end

  @spec commit(t) :: :ok
  def commit({:inmem, updates}) do
    Enum.each(updates, fn
      {utxo_key, :undefined} ->
	apply_op(:raw, {:delete, utxo_key})
      utxo_record ->
	apply_op(:raw, {:add, utxo_record})
    end)
    :ok
  end

  @spec block_changeset(Block.t) :: list(utxo_op)
  def block_changeset(%Block{txs: txs}) do
    Enum.flat_map(txs, &changeset/1)
  end

  @spec undo_block_changeset(t, Block.t) :: list(utxo_op)  
  def undo_block_changeset(db, %Block{txs: txs}) do
    Enum.flat_map(txs, fn tx -> undo_changeset(db, tx) end)
  end
  
  @spec changeset(TX.t) :: list(utxo_op)
  def changeset(tx = %TX{inputs: inputs, outputs: outputs}) do
    # TODO: ignore coinbase TX's    
    deleted = Enum.map(inputs, fn input -> {:delete, {input.tx_hash, input.offset}} end)
    added = Enum.with_index(outputs)
    |> Enum.map(fn {output, i} -> {:add, {{TX.hash(tx), i}, output}} end)
    deleted ++ added
  end

  @spec undo_changeset(t, TX.t) :: list(utxo_op)
  def undo_changeset(_db, tx = %TX{outputs: outputs, is_coinbase: true}) do
    Enum.with_index(outputs)
    |> Enum.map(fn {_, i} -> {:delete, {TX.hash(tx), i}} end)
  end
  
  def undo_changeset(db, tx = %TX{inputs: inputs, outputs: outputs}) do
    deleted = Enum.with_index(outputs)
    |> Enum.map(fn {_, i} -> {:delete, {TX.hash(tx), i}} end)
    added = Enum.map(inputs, fn input ->
      {:ok, output} = get(db, input.tx_hash, input.offset)
      {:add, {{input.tx_hash, input.offset}, output}}
    end)
    deleted ++ added
  end

  @spec scan(KeyStore.fingerprint) :: list(utxo_record)
  def scan(fingerprint) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.match_object({UTXODbTable, :_, {fingerprint, :_}}) end)
    result
  end

end

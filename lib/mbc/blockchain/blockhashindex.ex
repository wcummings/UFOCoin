defmodule MBC.Blockchain.BlockHashIndex do

  def init do
    :mnesia.create_table(BlockHashIndexTable, [attributes: [:block_hash, :offset], type: :set])
  end

  def insert(block_hash, offset) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({BlockHashIndexTable, block_hash, offset}) end)
    result
  end

  def get_offset(block_hash) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(BlockHashIndexTable, block_hash) end)
    case result do
      [{BlockHashIndexTable, ^block_hash, offset}] ->
	offset
      [] ->
	:undefined
    end
  end

end
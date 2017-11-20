defmodule MBC.Blockchain.BlockTable do

  @table_name :block_record
  
  def init do
    :mnesia.create_table(@table_name, [attributes: [:height_and_hash, :hash, :block], type: :ordered_set, index: [:hash]])
  end

  def insert(block) do
    {:atomic, result} = :mnesia.transaction(fn ->
      block_hash = MBC.Blockchain.Block.hash(block)
      :mnesia.write({@table_name, {block.height, block_hash}, block_hash, block})
    end)
    result
  end

  def get(block_hash) do
    case :mnesia.transaction(fn -> :mnesia.index_read(@table_name, block_hash, 1) end) do
      {:atomic, []} -> :undefined
      {:atomic, [block_record]} -> record_to_struct(block_record)
    end
  end

  def get_longest do
    {:atomic, records} = :mnesia.transaction(fn ->
      {block_height, _} = :mnesia.last(@table_name)
      :mnesia.match_object(@table_name, {@table_name, {block_height, :_}, :_, :_}, :read)
    end)
    Enum.map(records, &record_to_struct/1)
  end

  def record_to_struct({@table_name, _, _, block}) do
    block
  end
  
end

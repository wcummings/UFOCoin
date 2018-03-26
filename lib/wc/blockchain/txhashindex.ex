alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.TxHashIndex do
  @moduledoc """
  Map TX hashes to block hashes.
  """

  @spec init :: :ok
  def init do
    :tx_hash_index = :ets.new(:tx_hash_index, [:public, :named_set, :set])
    :ok
  end

  @spec insert(TX.tx_hash, BlockHeader.block_hash) :: :ok
  def insert(tx_hash, block_hash) do
    :true = :ets.insert_new(:tx_hash_index, {tx_hash, block_hash})
    :ok
  end

  @spec get_block_hash(TX.tx_hash) :: BlockHeader.block_hash
  def get_block_hash(tx_hash) do
    case :ets.lookup(:tx_hash_index, tx_hash) do
      [^tx_hash, block_hash] ->
	{:ok, block_hash}
      [] ->
	{:error, :notfound}
    end
  end
  
end

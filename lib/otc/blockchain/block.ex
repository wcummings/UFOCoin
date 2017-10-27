alias OTC.Blockchain.Types, as: Types
alias OTC.Blockchain.TX, as: TX

defmodule OTC.Blockchain.Block do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, txs: [], nonce: nil

  @type t :: %OTC.Blockchain.Block{prev_block_hash: Types.block_hash, difficulty: non_neg_integer(), txs: list(TX)}

  def serialize(block) do
    serialized_txs = Enum.map(block.txs, &TX.serialize/1)
    |> OTC.Util.binary_join
    <<block.prev_block_hash, block.difficulty, serialized_txs>>
  end

  def hash(block) do
    :crypto.hash(:sha256, serialize(block))
  end
  
end

require Logger

alias MBC.Blockchain.Types, as: Types
alias MBC.Blockchain.TX, as: TX

defmodule MBC.Blockchain.Block do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, txs: [], nonce: 0

  @type block_hash :: binary()
  @type t :: %MBC.Blockchain.Block{prev_block_hash: block_hash, difficulty: non_neg_integer(), txs: list(TX.t)}

  def encode(block = %MBC.Blockchain.Block{prev_block_hash: prev_block_hash, difficulty: difficulty, txs: txs, nonce: nonce}) do
    serialized_txs = Enum.map(txs, &TX.encode/1)
    prev_block_hash <> <<difficulty :: size(32)>> <> nonce <> <<length(serialized_txs) :: size(32)>> <> MBC.Util.binary_join(serialized_txs)
  end

  def hash(block) do
    :crypto.hash(:sha256, block)
  end

  def check_nonce(block = <<_ :: size(32), difficulty :: size(32), _ :: binary>>) do
    block_hash = hash(block)
    {:crypto.bytes_to_integer(block_hash) < MBC.Util.difficulty_to_target(difficulty), block_hash}
  end

  def update_nonce(encoded_block, new_nonce) do
    <<prev_block_hash :: size(32), difficulty :: size(32), nonce :: size(32), rest :: binary>> = encoded_block
    <<prev_block_hash :: size(32), difficulty :: size(32), new_nonce :: binary, rest :: binary>>
  end

end

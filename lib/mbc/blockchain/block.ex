require Logger

alias MBC.Blockchain.TX, as: TX

defmodule MBC.Blockchain.Block do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty, :height]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, txs: [], height: nil, nonce: <<0, 0, 0, 0>>

  @type block_hash :: binary()
  @type t :: %MBC.Blockchain.Block{prev_block_hash: block_hash, difficulty: non_neg_integer(), height: non_neg_integer(), txs: list(TX.t)}

  def encode(%MBC.Blockchain.Block{prev_block_hash: prev_block_hash, difficulty: difficulty, height: height, txs: txs, nonce: nonce}) do
    serialized_txs = Enum.map(txs, &TX.encode/1)
    prev_block_hash <> <<difficulty :: size(32)>> <> nonce <> <<length(serialized_txs) :: size(32)>> <> MBC.Util.binary_join(serialized_txs)
  end

  def hash(block = %MBC.Blockchain.Block{}) do
    encode(block) |> hash
  end
  
  def hash(block) do
    :crypto.hash(:sha256, block)
  end

  def check_nonce(block = %MBC.Blockchain.Block{}) do
    encode(block) |> check_nonce
  end
  
  def check_nonce(block = <<_ :: size(32), difficulty :: size(32), _ :: binary>>) do
    block_hash = hash(block)
    {:crypto.bytes_to_integer(block_hash) < MBC.Util.difficulty_to_target(difficulty), block_hash}
  end

  def update_nonce(encoded_block, new_nonce) do
    <<prev_block_hash :: size(32), difficulty :: size(32), _nonce :: size(32), rest :: binary>> = encoded_block
    <<prev_block_hash :: size(32), difficulty :: size(32), new_nonce :: binary, rest :: binary>>
  end

  # def from_block(%MBC.Blockchain.Block{block_hash: block_hash, height: prev_block_height}) do
  #   %MBC.Blockchain.Block{prev_block_hash: block_hash, height: prev_block_height + 1}
  # end

end

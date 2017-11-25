require Logger

alias MBC.Blockchain.TX, as: TX

defmodule MBC.Blockchain.Block do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty, :height]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, txs: [], height: nil, nonce: <<0, 0, 0, 0>>

  @type block_hash :: binary()
  @type t :: %MBC.Blockchain.Block{prev_block_hash: block_hash, difficulty: non_neg_integer(), height: non_neg_integer(), txs: list(TX.t)}

  def encode(%MBC.Blockchain.Block{prev_block_hash: prev_block_hash, timestamp: timestamp, difficulty: difficulty, height: height, txs: txs, nonce: nonce}) do
    serialized_txs = [<<>>] # Enum.map(txs, &TX.encode/1)
    prev_block_hash <> <<timestamp :: size(64)>> <> <<difficulty :: size(32)>> <> <<height :: size(32)>> <> nonce <> <<length(serialized_txs) :: size(32)>> <> MBC.Util.binary_join(serialized_txs)
  end

  def decode(<<prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), nonce :: size(32), tx_count :: size(32), rest :: binary>>) do
    %MBC.Blockchain.Block{prev_block_hash: <<prev_block_hash :: size(256)>>, timestamp: timestamp, difficulty: difficulty, height: height, nonce: <<nonce :: size(32)>>} # FIXME: TXs
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
  
  def check_nonce(block = <<_p :: size(256), _t :: size(64), difficulty :: size(32), _ :: binary>>) do
    block_hash = hash(block)
    {:crypto.bytes_to_integer(block_hash) < MBC.Util.difficulty_to_target(difficulty), block_hash}
  end

  def update_nonce(encoded_block, new_nonce) do
    <<prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), _nonce :: size(32), rest :: binary>> = encoded_block
    <<prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), new_nonce :: binary, rest :: binary>>
  end

  def next_block(block = %MBC.Blockchain.Block{height: prev_block_height}) do
    block_hash = hash(block)
    %MBC.Blockchain.Block{prev_block_hash: block_hash, height: prev_block_height + 1, difficulty: 20, timestamp: :os.system_time(:millisecond)}
  end

end

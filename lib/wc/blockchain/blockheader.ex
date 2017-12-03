require Logger

defmodule WC.Blockchain.BlockHeader do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty, :height]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, height: nil, nonce: <<0, 0, 0, 0>>

  @type block_hash :: binary
  @type encoded_block_header :: binary
  @type t :: %WC.Blockchain.BlockHeader{prev_block_hash: block_hash, difficulty: non_neg_integer(), height: non_neg_integer(), nonce: binary()}

  @spec encode(t) :: encoded_block_header
  def encode(%WC.Blockchain.BlockHeader{prev_block_hash: prev_block_hash, timestamp: timestamp, difficulty: difficulty, height: height, nonce: nonce}) do
    <<0x00, 0x01>> <> prev_block_hash <> <<timestamp :: size(64)>> <> <<difficulty :: size(32)>> <> <<height :: size(32)>> <> nonce
  end

  @spec decode(encoded_block_header) :: t
  def decode(<<0x00, 0x01, prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), nonce :: size(32)>>) do
    %WC.Blockchain.BlockHeader{prev_block_hash: <<prev_block_hash :: size(256)>>, timestamp: timestamp, difficulty: difficulty, height: height, nonce: <<nonce :: size(32)>>}
  end

  @spec hash(t) :: block_hash
  def hash(block_header = %WC.Blockchain.BlockHeader{}) do
    encode(block_header) |> hash
  end

  @spec hash(encoded_block_header) :: block_hash
  def hash(block_header) do
    :crypto.hash(:sha256, block_header)
  end

  @spec check_nonce(t) :: {non_neg_integer, block_hash}
  def check_nonce(block_header = %WC.Blockchain.BlockHeader{}) do
    encode(block_header) |> check_nonce
  end

  @spec check_nonce(encoded_block_header) :: {boolean, block_hash}  
  def check_nonce(block_header = <<0x00, 0x01, _p :: size(256), _t :: size(64), difficulty :: size(32), _ :: binary>>) do
    block_hash = hash(block_header)
    {:crypto.bytes_to_integer(block_hash) < WC.Util.difficulty_to_target(difficulty), block_hash}
  end

  @spec update_nonce(encoded_block_header, binary()) :: encoded_block_header
  def update_nonce(encoded_block_header, new_nonce) do
    <<0x00, 0x01, prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), _nonce :: size(32)>> = encoded_block_header
    <<0x00, 0x01, prev_block_hash :: size(256), timestamp :: size(64), difficulty :: size(32), height :: size(32), new_nonce :: binary>>
  end

end

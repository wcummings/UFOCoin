require Logger

defmodule WC.Blockchain.BlockHeader do
  @enforce_keys [:prev_block_hash, :timestamp, :difficulty, :height]
  defstruct prev_block_hash: nil, timestamp: nil, difficulty: nil, height: nil, nonce: <<0, 0, 0, 0>>

  @type block_hash :: binary
  @type encoded_block_header :: binary
  @type t :: %__MODULE__{prev_block_hash: block_hash, difficulty: non_neg_integer, height: non_neg_integer, nonce: binary}

  @spec encode(t) :: encoded_block_header
  def encode(%__MODULE__{prev_block_hash: <<prev_block_hash :: binary-size(32)>>, timestamp: timestamp, difficulty: difficulty, height: height, nonce: nonce}) do
    <<0x00, 0x01, prev_block_hash :: binary, timestamp :: size(64), difficulty :: size(32), height :: size(32), nonce :: binary>>
  end

  @spec decode(encoded_block_header) :: t
  def decode(<<0x00, 0x01, prev_block_hash :: binary-size(32), timestamp :: size(64), difficulty :: size(32), height :: size(32), nonce :: binary-size(4)>>) do
    %__MODULE__{prev_block_hash: prev_block_hash, timestamp: timestamp, difficulty: difficulty, height: height, nonce: nonce}
  end

  @spec hash(t) :: block_hash
  def hash(block_header = %__MODULE__{}) do
    encode(block_header) |> hash
  end

  @spec hash(encoded_block_header) :: block_hash
  def hash(block_header) do
    :crypto.hash(:sha256, block_header)
  end

  @spec check_nonce(t) :: {non_neg_integer, block_hash}
  def check_nonce(block_header = %__MODULE__{}) do
    encode(block_header) |> check_nonce
  end

  @spec check_nonce(encoded_block_header) :: {boolean, block_hash}  
  def check_nonce(block_header = <<0x00, 0x01, _p :: size(256), _t :: size(64), difficulty :: size(32), _ :: binary>>) do
    block_hash = hash(block_header)
    {:crypto.bytes_to_integer(block_hash) < WC.Util.difficulty_to_target(difficulty), block_hash}
  end

  @spec update_nonce(encoded_block_header, binary()) :: encoded_block_header
  def update_nonce(encoded_block_header, new_nonce) do
    <<0x00, 0x01, prev_block_hash :: binary-size(32), timestamp :: size(64), difficulty :: size(32), height :: size(32), _n :: binary-size(4)>> = encoded_block_header
    <<0x00, 0x01, prev_block_hash :: binary, timestamp :: size(64), difficulty :: size(32), height :: size(32), new_nonce :: binary>>
  end

  def pprint(block_header) do
    "[" <> Base.encode16(hash(block_header)) <> ", "
    <> "prev_block_hash: #{Base.encode16(block_header.prev_block_hash)}, "
    # <> "timestamp: #{block_header.timestamp}, "
    # <> "difficulty: #{block_header.difficulty}, "
    <> "height: #{block_header.height}, "
    # <> "nonce: #{inspect(block_header.nonce)}]"
  end

end

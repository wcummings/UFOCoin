alias WC.Blockchain.BlockHeader, as: BlockHeader

# TODO: this might belong in the P2P namespace
defmodule WC.Blockchain.InvItem do
  @enforce_keys [:type, :hash]
  defstruct [:type, :hash]

  @type itemtype :: :block
  @type encoded_invitem :: binary
  @type t :: %WC.Blockchain.InvItem{type: itemtype, hash: BlockHeader.block_hash}

  @spec encode(t) :: encoded_invitem
  def encode(%WC.Blockchain.InvItem{type: :block, hash: block_hash}) do
    <<0x00, block_hash :: binary>>
  end

  @spec decode(encoded_invitem) :: t
  def decode(<<0x00, block_hash :: binary>>) do
    %WC.Blockchain.InvItem{type: :block, hash: block_hash}
  end

  @spec from_block_hash(Block.block_hash) :: t
  def from_block_hash(block_hash) do
    %WC.Blockchain.InvItem{type: :block, hash: block_hash}
  end
  
end

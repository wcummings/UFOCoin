alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC.Blockchain.InvItem do
  @enforce_keys [:type, :hash]
  defstruct [:type, :hash]

  @type itemtype :: :block
  @type encoded_invitem :: binary
  @type t :: %WC.Blockchain.InvItem{type: itemtype, hash: BlockHeader.block_hash}

  @spec encode(t) :: encoded_invitem
  def encode(%WC.Blockchain.InvItem{type: :block, hash: block_hash}) do
    <<0x00, block_hash>>
  end

  @spec decode(encoded_invitem) :: t
  def decode(<<0x00, block_hash :: binary>>) do
    %WC.Blockchain.InvItem{type: :block, hash: block_hash}
  end
  
end

alias WC.P2P.Addr, as: P2PAddr
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.InvItem, as: InvItem

defmodule WC.P2P.Packet do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  @type encoded_packet :: binary
  @type version_p2p_packet :: %__MODULE__{proc: :version}
  @type versionack_p2p_packet :: %__MODULE__{proc: :versionack}
  @type addr_p2p_packet :: %__MODULE__{proc: :addr, extra_data: list(P2PAddr.t)}
  @type getaddrs_p2p_packet :: %__MODULE__{proc: :getaddrs}  
  @type ping_p2p_packet :: %__MODULE__{proc: :ping}
  @type pong_p2p_packet :: %__MODULE__{proc: :pong}
  @type block_p2p_packet :: %__MODULE__{proc: :block, extra_data: Block.t}
  @type inv_p2p_packet :: %__MODULE__{proc: :inv, extra_data: list(InvItem.t)}
  @type getblocks_p2p_packet :: %__MODULE__{proc: :getblocks, extra_data: list(BlockHeader.block_hash)}
  @type getdata_p2p_packet :: %__MODULE__{proc: :getdata, extra_data: list(InvItem.t)}
  @type t :: ping_p2p_packet | pong_p2p_packet | getaddrs_p2p_packet | addr_p2p_packet | version_p2p_packet | versionack_p2p_packet | block_p2p_packet | inv_p2p_packet | getblocks_p2p_packet | getdata_p2p_packet

  @spec decode(encoded_packet) :: t
  def decode(<<0x00, 0x01, length :: size(8), version :: binary-size(length)>>) do
    %__MODULE__{proc: :version, extra_data: version}
  end

  def decode(<<0x00, 0x02>>) do
    %__MODULE__{proc: :versionack}
  end

  def decode(<<0x00, 0x03, length :: size(16), addrs :: binary-size(length)>>) do
    addrs = for <<addr :: binary-size(6) <- addrs>>, do: P2PAddr.decode(addr)
    %__MODULE__{proc: :addr, extra_data: addrs}
  end

  def decode(<<0x00, 0x04>>) do
    %__MODULE__{proc: :getaddrs}
  end

  def decode(<<0x00, 0x05>>) do
    %__MODULE__{proc: :ping}
  end

  def decode(<<0x00, 0x06>>) do
    %__MODULE__{proc: :pong}
  end

  def decode(<<0x00, 0x07, length :: size(32), encoded_block :: binary-size(length)>>) do
    %__MODULE__{proc: :block, extra_data: Block.decode(encoded_block)}
  end

  def decode(<<0x00, 0x08, length :: size(16), invitems :: binary-size(length)>>) do
    invitems = for <<invitem :: binary-size(33) <- invitems>>, do: InvItem.decode(invitem)
    %__MODULE__{proc: :inv, extra_data: invitems}
  end

  def decode(<<0x00, 0x09, length :: size(32), block_hashes :: binary-size(length)>>) do
    block_hashes = for <<bh :: binary-size(32) <- block_hashes>>, do: bh
    %__MODULE__{proc: :getblocks, extra_data: block_hashes}
  end

  def decode(<<0x00, 0x10, length :: size(16), invitems :: binary-size(length)>>) do
      invitems = for <<invitem :: binary-size(33) <- invitems>>, do: InvItem.decode(invitem)
      %__MODULE__{proc: :getdata, extra_data: invitems}
  end
  
  @spec encode(t) :: encoded_packet
  def encode(%__MODULE__{proc: :version, extra_data: version}) do
    [<<0x00, 0x01>>,
     <<:erlang.iolist_size(version) :: size(8)>>,
     <<version :: binary>>]
  end

  def encode(%__MODULE__{proc: :versionack}) do
    <<0x00, 0x02>>
  end

  def encode(%__MODULE__{proc: :addr, extra_data: addrs}) do
    encoded_addrs = Enum.map(addrs, &P2PAddr.encode/1)
    [<<0x00, 0x03>>,
     <<:erlang.iolist_size(encoded_addrs) :: size(16)>>,
     encoded_addrs]
  end

  def encode(%__MODULE__{proc: :getaddrs}) do
    <<0x00, 0x04>>
  end

  def encode(%__MODULE__{proc: :ping}) do
    <<0x00, 0x05>>
  end

  def encode(%__MODULE__{proc: :pong}) do
    <<0x00, 0x06>>
  end

  def encode(%__MODULE__{proc: :block, extra_data: block}) do
    encoded_block = Block.encode(block)
    [<<0x00, 0x07,
     <<:erlang.iolist_size(encoded_block) :: size(32)>>,
     encoded_block :: binary>>]
  end

  def encode(%__MODULE__{proc: :inv, extra_data: invitems}) do
    encoded_invitems = Enum.map(invitems, &InvItem.encode/1)
    [<<0x00, 0x08>>,
     <<:erlang.iolist_size(encoded_invitems) :: size(16)>>,
     encoded_invitems]
  end

  def encode(%__MODULE__{proc: :getblocks, extra_data: block_hashes}) do
    [<<0x00, 0x09>>,
     <<:erlang.iolist_size(block_hashes) :: size(32)>>,
     block_hashes]
  end

  def encode(%__MODULE__{proc: :getdata, extra_data: invitems}) do
    encoded_invitems = Enum.map(invitems, &InvItem.encode/1)
    [<<0x00, 0x10>>,
     <<:erlang.iolist_size(encoded_invitems) :: size(16)>>,
     encoded_invitems]
  end

end

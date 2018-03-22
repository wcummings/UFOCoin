alias WC.P2P.Addr, as: P2PAddr
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.InvItem, as: InvItem

defmodule WC.P2P.Packet do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  @type encoded_packet :: binary
  @type version_p2p_packet :: %WC.P2P.Packet{proc: :version}
  @type versionack_p2p_packet :: %WC.P2P.Packet{proc: :versionack}
  @type addr_p2p_packet :: %WC.P2P.Packet{proc: :addr, extra_data: list(P2PAddr.t)}
  @type getaddrs_p2p_packet :: %WC.P2P.Packet{proc: :getaddrs}  
  @type ping_p2p_packet :: %WC.P2P.Packet{proc: :ping}
  @type pong_p2p_packet :: %WC.P2P.Packet{proc: :pong}
  @type block_p2p_packet :: %WC.P2P.Packet{proc: :block, extra_data: Block.t}
  @type inv_p2p_packet :: %WC.P2P.Packet{proc: :inv, extra_data: list(InvItem.t)}
  @type getblocks_p2p_packet :: %WC.P2P.Packet{proc: :getblocks, extra_data: list(BlockHeader.block_hash)}
  @type getdata_p2p_packet :: %WC.P2P.Packet{proc: :getdata, extra_data: list(InvItem.t)}
  @type t :: ping_p2p_packet | pong_p2p_packet | getaddrs_p2p_packet | addr_p2p_packet | version_p2p_packet | versionack_p2p_packet | block_p2p_packet | inv_p2p_packet | getblocks_p2p_packet | getdata_p2p_packet

  @spec decode(encoded_packet) :: t
  def decode(<<0x00, 0x01, version :: binary>>) do
    %WC.P2P.Packet{proc: :version, extra_data: version}
  end

  def decode(<<0x00, 0x02>>) do
    %WC.P2P.Packet{proc: :versionack}
  end

  def decode(<<0x00, 0x03, addrs :: binary>>) do
    addrs = for <<addr :: binary-size(6) <- addrs>>, do: P2PAddr.decode(addr)
    %WC.P2P.Packet{proc: :addr, extra_data: addrs}
  end

  def decode(<<0x00, 0x04>>) do
    %WC.P2P.Packet{proc: :getaddrs}
  end

  def decode(<<0x00, 0x05>>) do
    %WC.P2P.Packet{proc: :ping}
  end

  def decode(<<0x00, 0x06>>) do
    %WC.P2P.Packet{proc: :pong}
  end

  def decode(<<0x00, 0x07, encoded_block :: binary>>) do
    %WC.P2P.Packet{proc: :block, extra_data: Block.decode(encoded_block)}
  end

  def decode(<<0x00, 0x08, invitems :: binary>>) do
    invitems = for <<invitem :: binary-size(33) <- invitems>>, do: InvItem.decode(invitem)
    %WC.P2P.Packet{proc: :inv, extra_data: invitems}
  end

  def decode(<<0x00, 0x09, block_hashes :: binary>>) do
    block_hashes = for <<bh :: binary-size(32) <- block_hashes>>, do: bh
    %WC.P2P.Packet{proc: :getblocks, extra_data: block_hashes}
  end

  def decode(<<0x00, 0x10, invitems :: binary>>) do
      invitems = for <<invitem :: binary-size(33) <- invitems>>, do: InvItem.decode(invitem)
      %WC.P2P.Packet{proc: :getdata, extra_data: invitems}
  end
  
  @spec encode(t) :: encoded_packet
  def encode(%WC.P2P.Packet{proc: :version, extra_data: version}) do
    <<0x00, 0x01, version :: binary>>
  end

  def encode(%WC.P2P.Packet{proc: :versionack}) do
    <<0x00, 0x02>>
  end

  def encode(%WC.P2P.Packet{proc: :addr, extra_data: addr_list}) do
    [<<0x00, 0x03>>, Enum.map(addr_list, &P2PAddr.encode/1)]
  end

  def encode(%WC.P2P.Packet{proc: :getaddrs}) do
    <<0x00, 0x04>>
  end

  def encode(%WC.P2P.Packet{proc: :ping}) do
    <<0x00, 0x05>>
  end

  def encode(%WC.P2P.Packet{proc: :pong}) do
    <<0x00, 0x06>>
  end

  def encode(%WC.P2P.Packet{proc: :block, extra_data: block}) do
    encoded_block = Block.encode(block)
    <<0x00, 0x07, encoded_block :: binary>>
  end

  def encode(%WC.P2P.Packet{proc: :inv, extra_data: invitems}) do
    [<<0x00, 0x08>>, Enum.map(invitems, &InvItem.encode/1)]
  end

  def encode(%WC.P2P.Packet{proc: :getblocks, extra_data: block_hashes}) do
    [<<0x00, 0x09>>, block_hashes]
  end

  def encode(%WC.P2P.Packet{proc: :getdata, extra_data: invitems}) do
    [<<0x00, 0x10>>, Enum.map(invitems, &InvItem.encode/1)]
  end
  
end

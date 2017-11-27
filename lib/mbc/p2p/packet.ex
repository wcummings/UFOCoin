alias MBC.P2P.Addr, as: P2PAddr
alias MBC.Blockchain.Block, as: Block

defmodule MBC.P2P.Packet do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  @type version_p2p_packet :: %MBC.P2P.Packet{proc: :version}
  @type versionack_p2p_packet :: %MBC.P2P.Packet{proc: :versionack}
  @type addr_p2p_packet :: %MBC.P2P.Packet{proc: :addr, extra_data: list(P2PAddr.t)}
  @type getaddrs_p2p_packet :: %MBC.P2P.Packet{proc: :getaddrs}  
  @type ping_p2p_packet :: %MBC.P2P.Packet{proc: :ping}
  @type pong_p2p_packet :: %MBC.P2P.Packet{proc: :pong}
  @type block_p2p_packet :: %MBC.P2P.Packet{proc: :block, extra_data: Block.t}
  @type t :: ping_p2p_packet | pong_p2p_packet | getaddrs_p2p_packet | addr_p2p_packet | version_p2p_packet | versionack_p2p_packet | block_p2p_packet

  def decode(<<0x00, 0x01, version :: binary>>) do
    %MBC.P2P.Packet{proc: :version, extra_data: version}
  end

  def decode(<<0x00, 0x02>>) do
    %MBC.P2P.Packet{proc: :versionack}
  end

  def decode(<<0x00, 0x03, addr_list :: binary>>) do
    %MBC.P2P.Packet{proc: :addr, extra_data: decode_addr_list(addr_list)}
  end

  def decode(<<0x00, 0x04>>) do
    %MBC.P2P.Packet{proc: :getaddrs}
  end

  def decode(<<0x00, 0x05>>) do
    %MBC.P2P.Packet{proc: :ping}
  end

  def decode(<<0x00, 0x06>>) do
    %MBC.P2P.Packet{proc: :pong}
  end

  def decode(<<0x00, 0x07, encoded_block :: binary>>) do
    %MBC.P2P.Packet{proc: :block, extra_data: Block.decode(encoded_block)}
  end
  
  def decode_addr_list(bin) do
    decode_addr_list(bin, [])
  end

  # FIXME: size(6) = size of P2PAddr.encode/1 output, can handle this with a macro
  def decode_addr_list(<<addr :: binary - size(6), rest :: binary>>, acc) do
    decode_addr_list(rest, [P2PAddr.decode(addr)|acc])
  end
  
  def decode_addr_list(<<>>, acc) do
    acc
  end

  def encode(%MBC.P2P.Packet{proc: :version, extra_data: version}) do
    <<0x00, 0x01, version :: binary>>
  end

  def encode(%MBC.P2P.Packet{proc: :versionack}) do
    <<0x00, 0x02>>
  end

  def encode(%MBC.P2P.Packet{proc: :addr, extra_data: addr_list}) do
    [<<0x00, 0x03>>, Enum.map(addr_list, &P2PAddr.encode/1)]
  end

  def encode(%MBC.P2P.Packet{proc: :getaddrs}) do
    <<0x00, 0x04>>
  end

  def encode(%MBC.P2P.Packet{proc: :ping}) do
    <<0x00, 0x05>>
  end

  def encode(%MBC.P2P.Packet{proc: :pong}) do
    <<0x00, 0x06>>
  end

  def encode(%MBC.P2P.Packet{proc: :block, extra_data: block}) do
    encoded_block = Block.encode(block)
    <<0x00, 0x07, encoded_block :: binary>>
  end
  
end

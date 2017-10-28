require Logger

defmodule MBC.P2P.Packet do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  @type ping_p2p_packet :: %MBC.P2P.Packet{proc: :ping}
  @type pong_p2p_packet :: %MBC.P2P.Packet{proc: :pong}
  @type getaddrs_p2p_packet :: %MBC.P2P.Packet{proc: :getaddrs}
  @type addr_p2p_packet :: %MBC.P2P.Packet{proc: :addr, extra_data: list(MBC.P2P.Addr.t)}
  @type version_p2p_packet :: %MBC.P2P.Packet{proc: :version, extra_data: String.t}
  @type versionack_p2p_packet :: %MBC.P2P.Packet{proc: :versionack}
  @type t :: ping_p2p_packet | pong_p2p_packet | getaddrs_p2p_packet | addr_p2p_packet | version_p2p_packet | versionack_p2p_packet

  def encode(request) do
    :erlang.term_to_binary(request)
  end
  
  def decode(data) do
    request = :erlang.binary_to_term(data)
    {:ok, proc} = Map.fetch(request, :proc)
    extra_data = Map.get(request, :extra_data, [])
    %MBC.P2P.Packet{proc: proc, extra_data: extra_data}
  end 
  
end

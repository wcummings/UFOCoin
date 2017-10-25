defmodule OTC.P2P.Packet do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  # Whitelist of values to be turned into atoms
  @allowed_proc_values ["ping", "getaddrs", "addr", "version", "versionack"]
  
  @type ping_p2p_packet :: %OTC.P2P.Packet{proc: :ping}
  @type getaddrs_p2p_packet :: %OTC.P2P.Packet{proc: :getaddrs}
  @type addr_p2p_packet :: %OTC.P2P.Packet{proc: :addr, extra_data: list(OTC.P2P.Addr.t)}
  @type version_p2p_packet :: %OTC.P2P.Packet{proc: :version, extra_data: String.t}
  @type versionack_p2p_packet :: %OTC.P2P.Packet{proc: :versionack}
  @type t :: ping_p2p_packet | getaddrs_p2p_packet | addr_p2p_packet | version_p2p_packet | versionack_p2p_packet

  # @spec encode(t) :: :msgpack.object
  def encode(request) do
    # :msgpack.pack(request, [{:known_atoms, [:"OTC.P2P.Addr"]}])
    :erlang.term_to_binary(request)
  end
  
  # @spec decode(:msgpack.object) :: t
  def decode(data) do
    # {:ok, request} = :msgpack.unpack(data, [{:known_atoms, [:"OTC.P2P.Addr"]}])
    request = :erlang.binary_to_term(data)
    {:ok, proc} = Map.fetch(request, :proc)
    # true = Enum.member?(@allowed_proc_values, proc)
    extra_data = Map.get(request, :extra_data, [])
    %OTC.P2P.Packet{proc: proc, extra_data: extra_data}
  end
  
end

defmodule OTC.P2PPacket do
  @enforce_keys [:proc]
  defstruct proc: nil, extra_data: []

  # Whitelist of values to be turned into atoms
  @allowed_proc_values ["ping", "getaddrs", "addr", "version", "versionack"]
  
  @type ping_rpc_packet :: %OTC.P2PPacket{proc: :ping}
  @type getaddrs_rpc_packet :: %OTC.P2PPacket{proc: :getaddrs}
  @type addr_rpc_packet :: %OTC.P2PPacket{proc: :addr, extra_data: list(non_neg_integer)}
  @type t :: ping_rpc_packet | getaddrs_rpc_packet | addr_rpc_packet

  @spec encode(t) :: :msgpack.object
  def encode(request) do
    :msgpack.pack(request)
  end
  
  @spec decode(:msgpack.object) :: t
  def decode(data) do
    {:ok, request} = :msgpack.unpack(data)
    {:ok, proc} = Map.fetch(request, "proc")
    true = Enum.member?(@allowed_proc_values, proc)
    extra_data = Map.get(request, "extra_data", [])
    %OTC.P2PPacket{proc: String.to_atom(proc), extra_data: extra_data}
  end
  
end

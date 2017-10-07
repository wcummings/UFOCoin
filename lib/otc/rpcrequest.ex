defmodule OTC.RPCRequest do
  @enforce_keys [:reqid, :proc]
  defstruct reqid: nil, proc: nil, extra_data: []

  # Whitelist of values to be turned into atoms
  @allowed_proc_values ["heartbeat", "listpeers", "addr"]
  
  @type heartbeat_rpc_request :: %OTC.RPCRequest{reqid: non_neg_integer, proc: :heartbeat}
  @type listpeers_rpc_request :: %OTC.RPCRequest{reqid: non_neg_integer, proc: :listpeers}
  @type addr_rpc_request :: %OTC.RPCRequest{reqid: non_neg_integer, proc: :addr, extra_data: [non_neg_integer]}
  @type t :: heartbeat_rpc_request | listpeers_rpc_request | addr_rpc_request

  @spec encode(t) :: :msgpack.object
  def encode(request) do
    :msgpack.pack(request)
  end
  
  @spec decode(:msgpack.object) :: t
  def decode(data) do
    {ok, request} = :msgpack.unpack(data)
    {:ok, reqid} = Map.fetch(request, "reqid")
    {:ok, proc} = Map.fetch(request, "proc")
    :true = Enum.member?(@allowed_proc_values, proc)
    extra_data = Map.get(request, "extra_data", [])
    %OTC.RPCRequest{reqid: reqid, proc: String.to_atom(proc), extra_data: extra_data}
  end
  
end

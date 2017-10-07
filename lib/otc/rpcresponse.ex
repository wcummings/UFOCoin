defmodule OTC.RPCResponse do
  @enforce_keys [:reqid, :errors, :result]
  defstruct [:reqid, :errors, :result]

  @type heartbeat_rpc_response :: %OTC.RPCResponse{reqid: non_neg_integer, errors: [], result: String.t}
  @type listpeers_rpc_response :: %OTC.RPCResponse{reqid: non_neg_integer, errors: [], result: list(non_neg_integer)}
  @type t :: heartbeat_rpc_response | listpeers_rpc_response

  @spec encode(t) :: :msgpack.object
  def encode(data) do
    :msgpack.pack(data)
  end

  @spec decode(:msgpack.object) :: t
  def decode(data) do
    {:ok, request} = :msgpack.unpack(data)
    {:ok, reqid} = Map.fetch(request, "reqid")
    {:ok, errors} = Map.fetch(request, "errors")
    {:ok, result} = Map.fetch(request, "result")
    %OTC.RPCResponse{reqid: reqid, errors: errors, result: result}
  end
  
end

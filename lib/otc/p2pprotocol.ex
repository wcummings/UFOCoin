require Logger

defmodule OTC.P2PProtocol do
  use GenServer

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, _opts) do
    Logger.info "New connection"    
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(ref, socket, transport) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}, {:packet, 4}, :binary])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
  end

  def handle_info({:tcp, socket, data}, state = %{socket: socket, transport: transport}) do
    request = OTC.RPCRequest.decode(data)
    Logger.info "Executing #(request.proc)"
    state = handle_rpc(request, state)
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    transport.close(socket)
    {:stop, :normal, state}
  end

  def handle_rpc(%OTC.RPCRequest{proc: :version, extra_data: extra_data}, state = %{socket: socket, transport: transport}) do
    reqid = UUID.uuid1
    request = %OTC.RPCRequest{reqid: reqid, proc: :version, extra_data: %{"vsn": OTC.version}}
    payload = OTC.RPCResponse.encode(request)
    transport.send(socket, payload)
    if extra_data["vsn"] == OTC.version do
      request = %OTC.RPCRequest{reqid: reqid, proc: :versionack}
      payload = OTC.RPCResponse.encode(request)
      transport.send(socket, payload)
    end
    state
  end
  
  def handle_rpc(%OTC.RPCRequest{reqid: reqid, proc: :heartbeat, extra_data: []}, state = %{socket: socket, transport: transport}) do
    response = %OTC.RPCResponse{reqid: reqid, errors: [], result: "heartbeat"}    
    payload = OTC.RPCResponse.encode(response)
    transport.send(socket, payload)
    state
  end

  def handle_rpc(%OTC.RPCRequest{reqid: reqid, proc: :listpeers}, state = %{socket: socket, transport: transport}) do
    {:ok, peers} = Database.Peer.get_peers
    response = %OTC.RPCResponse{reqid: reqid, errors: [], result: peers}
    payload = OTC.RPCResponse.encode(response)
    transport.send(socket, payload)
    state
  end

  def handle_rpc({:addr, addr}, state) do
    # TODO: verify addr, then broadcast to existing peers
    Database.Peer.add_peer(addr)
    state
  end

  def handle_rpc(request, state) do
    Logger.warn "Invalid request "
    state
  end
end

require Logger

defmodule OBC.P2PProtocol do
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
    {:ok, request} = :msgpack.unpack(data)
    {:ok, proc} = Map.fetch(request, "proc")
    {:ok, reqid} = Map.fetch(request, "reqid")
    args = Map.get(request, "args", [])
    state = handle_rpc({proc, args, reqid}, state)
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    transport.close(socket)
    {:stop, :normal, state}
  end

  def handle_rpc({"heartbeat", [], reqid}, state = %{socket: socket, transport: transport}) do
    Logger.info "Heartbeat"
    payload = :msgpack.pack(%{reqid: reqid, result: "heartbeat"})
    transport.send(socket, payload)
    state
  end
  
  def handle_rpc({"listpeers", [], reqid}, state = %{socket: socket, transport: transport}) do
    {:ok, peers} = Peer.get_peers
    payload = :msgpack.pack(%{reqid: reqid, result: peers})
    transport.send(socket, payload)
    state
  end

  def handle_rpc({:addr, addr}, state) do
    # TODO: verify addr, then broadcast to existing peers
    Peer.add_peer(addr)
    state
  end

  def handle_rpc(_, state) do
    Logger.warn "Invalid RPC"
    state
  end
end

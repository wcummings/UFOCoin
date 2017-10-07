require Logger

defmodule OTC.P2PClient do
  use GenServer

  @initial_state %{
    pending_reqs: %{},
    host: nil,
    port: nil,
    socket: nil
  }
  
  def start_link(host, port) do
    GenServer.start_link(__MODULE__, %{@initial_state | host: host, port: port})
  end

  def version(pid) do
    :gen_server.cast(pid, :version)
  end
  
  def heartbeat(pid) do
    :gen_server.call(pid, :heartbeat)
  end

  def listpeers(pid) do
    :gen_server.call(pid, :listpeers)
  end
  
  def init(state = %{host: host, port: port}) do
    {:ok, socket} = :gen_tcp.connect(host, port, [:binary, active: true, packet: 4])
    {:ok, %{state | socket: socket}}
  end

  def handle_cast({:version, version}, state = %{socket: socket}) do
    reqid = UUID.uuid1(:hex)
    request = %OTC.RPCRequest{reqid: reqid, proc: :version, extra_data: #{"vsn": OTC.version}}
    payload = OTC.RPCRequest.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}
  end

  def handle_call(:listpeers, from, state = %{socket: socket}) do
    reqid = UUID.uuid1(:hex)
    request = %OTC.RPCRequest{reqid: reqid, proc: :listpeers}
    payload = OTC.RPCRequest.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, put_in(state.pending_reqs[:reqid], from)}
  end

  def handle_call(:heartbeat, from, state = %{socket: socket}) do
    Logger.info "Client call: heartbeat"
    reqid = UUID.uuid1(:hex)
    request = %OTC.RPCRequest{reqid: reqid, proc: :heartbeat}
    payload = OTC.RPCRequest.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, put_in(state.pending_reqs[:reqid], from)}
  end
    
  def handle_info({:tcp, socket, data}, state = %{socket: socket, pending_reqs: pending_reqs}) do
    response = OTC.RPCResponse.decode(data)
    reqid = response.reqid
    if Map.has_key?(pending_reqs, :reqid) do
      :gen_server.reply(pending_reqs[:reqid], response)
    end
    {:noreply, %{state | pending_reqs: Map.delete(pending_reqs, reqid)}}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :normal, state}
  end
    
end

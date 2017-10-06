require Logger

defmodule OBC.P2PClient do
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

  def handle_call(:listpeers, from, state = %{socket: socket, pending_reqs: pending_reqs}) do
    Logger.info "Client call: listpeers"
    reqid = UUID.uuid1(:hex)
    payload = :msgpack.pack(%{proc: "listpeers", reqid: reqid})
    request = <<byte_size(payload) :: size(32), payload :: binary>>
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, %{state | pending_reqs: put_in(pending_reqs[:reqid], from)}}
  end

  def handle_call(:heartbeat, from, state = %{socket: socket, pending_reqs: pending_reqs}) do
    Logger.info "Client call: heartbeat"
    reqid = UUID.uuid1(:hex)
    payload = :msgpack.pack(%{proc: "heartbeat", reqid: reqid})
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, %{state | pending_reqs: put_in(pending_reqs[:reqid], from)}}
  end
    
  def handle_info({:tcp, socket, data}, state = %{socket: socket, pending_reqs: pending_reqs}) do
    {:ok, response} = :msgpack.unpack(data)
    # TODO: maybe validate response
    # {:ok, code} = Map.fetch(response, :code)
    # result = response[:result]
    reqid = Map.fetch(response, :reqid)
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

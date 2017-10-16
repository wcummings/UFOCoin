require Logger
require Amnesia
require Amnesia.Helper

defmodule OTC.P2P.Protocol do
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

  def handle_info({:tcp, _socket, data}, state) do
    request = OTC.P2P.Packet.decode(data)
    Logger.info "Received #{request.proc}"
    state = handle_packet(request, state)
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    transport.close(socket)
    {:stop, :normal, state}
  end

  def handle_packet(%OTC.P2P.Packet{proc: :version, extra_data: extra_data}, state = %{socket: socket, transport: transport}) do
    request = %OTC.P2P.Packet{proc: :version, extra_data: %{"vsn": OTC.version}}
    payload = OTC.P2P.Packet.encode(request)
    transport.send(socket, payload)
    if extra_data["vsn"] == OTC.version do
      request = %OTC.P2P.Packet{proc: :versionack}
      payload = OTC.P2P.Packet.encode(request)
      transport.send(socket, payload)
    end
    state
  end
  
  def handle_packet(%OTC.P2P.Packet{proc: :ping, extra_data: []}, state = %{socket: socket, transport: transport}) do
    response = %OTC.P2P.Packet{proc: :pong}
    payload = OTC.P2P.Packet.encode(response)
    transport.send(socket, payload)
    state
  end

  def handle_packet(%OTC.P2P.Packet{proc: :getaddrs}, state = %{socket: socket, transport: transport}) do
    addrs = OTC.P2P.AddrTable.get_addrs
    response = %OTC.P2P.Packet{proc: :addr, extra_data: addrs}
    payload = OTC.P2P.Packet.encode(response)
    transport.send(socket, payload)
    state
  end

  def handle_packet(%OTC.P2P.Packet{proc: :addr, extra_data: addrs}, state) do
    OTC.P2P.AddrTable.add_addrs(addrs)
    state
  end

  def handle_packet(_, state) do
    Logger.warn "Invalid request"
    state
  end
end

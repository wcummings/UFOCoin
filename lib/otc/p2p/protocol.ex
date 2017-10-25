require Logger

alias OTC.P2P.Packet, as: P2PPacket
alias OTC.P2P.AddrTable, as: P2PAddrTable

defmodule OTC.P2P.Protocol do
  use GenServer

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, _opts) do
    {:ok, {address, port}} = :inet.peername(socket)
    address_string = :inet.ntoa(address)
    Logger.info "New connection from #{address_string}:#{port}"
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(ref, socket, transport) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}, {:packet, 4}, :binary])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
  end

  def handle_info({:tcp, _socket, data}, state) do
    request = P2PPacket.decode(data)
    Logger.info "Received command #{inspect(request)}"
    state = handle_packet(request, state)
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    # {:ok, {address, port}} = :inet.peername(socket)
    # address_string = :inet.ntoa(address)
    # Logger.info "Closing connection from #{address_string}:#{port}"
    :ok = transport.close(socket)
    {:stop, :normal, state}
  end

  def handle_packet(%P2PPacket{proc: :version, extra_data: version_string}, state = %{socket: socket, transport: transport}) do
    request = %P2PPacket{proc: :version, extra_data: OTC.version}
    payload = P2PPacket.encode(request)
    :ok = transport.send(socket, payload)
    if version_string == OTC.version do
      request = %P2PPacket{proc: :versionack}
      payload = P2PPacket.encode(request)
      transport.send(socket, payload)
    end
    state
  end
  
  def handle_packet(%P2PPacket{proc: :ping}, state = %{socket: socket, transport: transport}) do
    response = %P2PPacket{proc: :pong}
    payload = P2PPacket.encode(response)
    transport.send(socket, payload)
    state
  end

  def handle_packet(%P2PPacket{proc: :getaddrs}, state = %{socket: socket, transport: transport}) do
    addrs = P2PAddrTable.get_addrs
    response = %P2PPacket{proc: :addr, extra_data: addrs}
    payload = P2PPacket.encode(response)
    Logger.info "Sending addrs: #{inspect(response)}"
    transport.send(socket, payload)
    state
  end

  def handle_packet(%P2PPacket{proc: :addr, extra_data: addrs}, state) do
    ip = Application.get_env(:otc, :ip)
    port = Application.get_env(:otc, :port)
    Enum.filter(addrs, fn (addr) -> {addr.ip, addr.port} != {ip, port} end) 
    |> P2PAddrTable.add_addrs
    state
  end

  def handle_packet(_, state) do
    Logger.warn "Invalid request"
    state
  end
  
end

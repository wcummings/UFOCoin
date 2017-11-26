require Logger

alias MBC.Blockchain.LogServer, as: LogServer
alias MBC.Blockchain.Block, as: Block
alias MBC.P2P.Packet, as: P2PPacket
alias MBC.P2P.AddrTable, as: P2PAddrTable
alias MBC.P2P.PingFSM, as: P2PPingFSM
alias MBC.P2P.AddrServer, as: P2PAddrServer

defmodule MBC.P2P.Protocol do
  use GenServer

  # TODO: use one value in protocol.ex and clientfsm.ex
  @handshake_timeout_ms 10000
  
  @behaviour :ranch_protocol

  @initial_state %{
    socket: nil,
    transport: nil,
    connected: false,
    pingfsm: nil
  }

  def start_link(ref, socket, transport, _opts) do
    {:ok, {address, port}} = :inet.peername(socket)
    address_string = :inet.ntoa(address)
    Logger.info "New connection from #{address_string}:#{port}"
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def send_packet(pid, packet = %P2PPacket{}) do
    GenServer.cast(pid, {:send_packet, packet})
  end

  def init(ref, socket, transport) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, :once}, {:packet, 4}, :binary])
    :gen_server.enter_loop(__MODULE__, [], %{@initial_state | socket: socket, transport: transport}, @handshake_timeout_ms)
  end

  def handle_cast({:send_packet, packet}, state = %{socket: socket, transport: transport}) do
    payload = P2PPacket.encode(packet)
    :ok = transport.send(socket, payload)
    {:noreply, state}
  end
    
  def handle_info({:tcp, _socket, data}, state = %{socket: socket, transport: transport}) do
    request = P2PPacket.decode(data)
    Logger.info "Received command #{inspect(request)}"
    state = handle_packet(request, state)
    :ok = transport.setopts(socket, [{:active, :once}, {:packet, 4}, :binary])    
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    # {:ok, {address, port}} = :inet.peername(socket)
    # address_string = :inet.ntoa(address)
    # Logger.info "Closing connection from #{address_string}:#{port}"
    :ok = transport.close(socket)
    {:stop, :normal, state}
  end

  # Handshake timeout
  def handle_info(:timeout, %{connected: false}) do
    {:stop, :normal}
  end

  def handle_info(:timeout, state), do: {:noreply, state}

  def handle_packet(%P2PPacket{proc: :version, extra_data: version_string}, state = %{socket: socket, transport: transport}) do
    request = %P2PPacket{proc: :version, extra_data: MBC.version}
    payload = P2PPacket.encode(request)
    :ok = transport.send(socket, payload)
    if version_string == MBC.version do
      request = %P2PPacket{proc: :versionack}
      payload = P2PPacket.encode(request)
      transport.send(socket, payload)
      # Handshake complete, lets start pinging
      {:ok, pid} = P2PPingFSM.start_link(self())
      %{state | pingfsm: pid, connected: true}
    else
      state
    end
  end
  
  def handle_packet(%P2PPacket{proc: :pong}, state = %{pingfsm: pingfsm, connected: true}) do
    P2PPingFSM.pong(pingfsm)
    state
  end

  def handle_packet(%P2PPacket{proc: :getaddrs}, state = %{socket: socket, transport: transport, connected: true}) do
    addrs = P2PAddrTable.get_all
    response = %P2PPacket{proc: :addr, extra_data: addrs}
    payload = P2PPacket.encode(response)
    # Logger.info "Sending addrs: #{inspect(response)}"
    transport.send(socket, payload)
    state
  end

  def handle_packet(%P2PPacket{proc: :addr, extra_data: addrs}, state = %{connected: true}) do
    ip = Application.get_env(:mbc, :ip)
    port = Application.get_env(:mbc, :port)
    Enum.filter(addrs, fn addr -> {addr.ip, addr.port} != {ip, port} end) 
    |> P2PAddrTable.insert
    state
  end

  def handle_packet(packet = %P2PPacket{proc: :block, extra_data: block}, state = %{connected: true}) do
    case LogServer.get_block_by_hash(Block.hash(block)) do
      {:error, :notfound} ->
	state
      {:ok, _block} -> 
	case Block.validate_block(block) do
	  :ok ->
	    # Broadcast block if we haven't seen it before
	    P2PAddrServer.broadcast(packet)
	    LogServer.update(block)
	  {:error, error} ->
	    Logger.warn "Block rejected, reason: #{inspect(error)}, block: #{inspect(block)}"
	end
	state
    end
  end

  def handle_packet(request, state) do
    Logger.warn "Invalid request: #{inspect(request)}"
    state
  end

end

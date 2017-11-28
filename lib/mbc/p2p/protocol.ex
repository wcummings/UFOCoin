require Logger

alias MBC.P2P.Packet, as: P2PPacket
alias MBC.P2P.ConnectionSupervisor, as: P2PConnectionSupervisor

defmodule MBC.P2P.Protocol do
  use GenServer
  @behaviour :ranch_protocol

  @initial_state %{
    socket: nil,
    transport: nil,
    connected: false,
    connection: nil
  }

  # TODO: use one value in protocol.ex and clientfsm.ex
  @handshake_timeout_ms 10000
  
  @socket_opts [:binary, packet: 4, active: :once]

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
    :ok = transport.setopts(socket, @socket_opts)
    :gen_server.enter_loop(__MODULE__, [], %{@initial_state | socket: socket, transport: transport}, @handshake_timeout_ms)
  end

  def handle_info({:tcp, _socket, data}, state = %{socket: socket, transport: transport}) do
    request = P2PPacket.decode(data)
    Logger.info "Received packet #{inspect(request)}"
    state = handle_packet(request, state)
    :ok = transport.setopts(socket, @socket_opts)    
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
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
      :ok = transport.send(socket, payload)
      {:ok, pid} = P2PConnectionSupervisor.new_connection(socket)
      :true = Process.link(pid)
      :ok = :gen_tcp.controlling_process(socket, pid)
      %{state | connection: pid, connected: true}
    else
      state
    end
  end

end

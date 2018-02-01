require Logger

alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.ConnectionSupervisor, as: P2PConnectionSupervisor

defmodule WC.P2P.Handshake do
  use GenServer

  @initial_state %{
    socket: nil,
    connected: false,
    connection: nil
  }

  # TODO: use one value in handshake.ex and clientfsm.ex
  @handshake_timeout_ms 10000
  
  @socket_opts [:binary, packet: 4, active: :once]

  def start_link(opts, socket) do
    GenServer.start_link(__MODULE__, [socket], opts)
  end
  
  def init([socket]) do
    {:ok, {address, port}} = :inet.peername(socket)
    address_string = :inet.ntoa(address)
    Logger.info "New connection from #{address_string}:#{port}"
    :ok = :inet.setopts(socket, @socket_opts)
    {:ok, %{@initial_state | socket: socket}, @handshake_timeout_ms}
  end

  def handle_info({:tcp, _socket, data}, state = %{socket: socket}) do
    request = P2PPacket.decode(data)
    state = handle_packet(request, state)
    :ok = :inet.setopts(socket, [active: :once])
    {:noreply, state}
  end
  
  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :ok = :gen_tcp.close(socket)
    {:stop, :normal, state}
  end

  # Handshake timeout
  def handle_info(:timeout, %{connected: false}) do
    {:stop, :normal}
  end

  def handle_info(:timeout, state), do: {:noreply, state}

  def handle_packet(%P2PPacket{proc: :version, extra_data: version_string}, state = %{socket: socket, connected: false}) do
    request = %P2PPacket{proc: :version, extra_data: WC.version}
    payload = P2PPacket.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    if version_string == WC.version do
      request = %P2PPacket{proc: :versionack}
      payload = P2PPacket.encode(request)
      :ok = :gen_tcp.send(socket, payload)
      {:ok, pid} = P2PConnectionSupervisor.new_connection(socket)
      :true = Process.link(pid)
      :ok = :gen_tcp.controlling_process(socket, pid)
      %{state | connection: pid, connected: true}
    else
      state
    end
  end

end

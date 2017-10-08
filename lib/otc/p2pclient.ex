require Logger

defmodule OTC.P2PClient do
  use GenServer

  @initial_state %{
    host: nil,
    port: nil,
    socket: nil,
    pid: nil
  }
  
  def start_link(pid, host, port) do
    GenServer.start(__MODULE__, [pid, host, port], [])
  end

  def version(pid) do
    :gen_server.cast(pid, :version)
  end
  
  def ping(pid) do
    :gen_server.cast(pid, :ping)
  end

  def getaddrs(pid) do
    :gen_server.cast(pid, :getaddrs)
  end
  
  def init([pid, host, port]) do
    {:ok, socket} = :gen_tcp.connect(host, port, [:binary, active: true, packet: 4])
    {:ok, %{@initial_state | socket: socket, host: host, port: port, pid: pid}}
  end

  def handle_cast(:version, state = %{socket: socket}) do
    Logger.info "Sending version message"
    request = %OTC.P2PPacket{proc: :version, extra_data: %{"vsn": OTC.version}}
    payload = OTC.P2PPacket.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}
  end

  def handle_cast(:getaddrs, state = %{socket: socket}) do
    Logger.info "Sending getaddrs message"
    request = %OTC.P2PPacket{proc: :getaddrs}
    payload = OTC.P2PPacket.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}
  end

  def handle_cast(:ping, state = %{socket: socket}) do
    request = %OTC.P2PPacket{proc: :ping}
    payload = OTC.P2PPacket.encode(request)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}
  end
    
  def handle_info({:tcp, socket, data}, state = %{socket: socket}) do
    response = OTC.P2PPacket.decode(data)
    send state.pid, response
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :normal, state}
  end
    
end

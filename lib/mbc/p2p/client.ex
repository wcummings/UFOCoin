require Logger

alias MBC.P2P.Packet, as: P2PPacket
alias MBC.P2P.Addr, as: P2PAddr

defmodule MBC.P2P.Client do
  use GenServer

  @initial_state %{
    ip: nil,
    port: nil,
    socket: nil,
    pid: nil
  }

  @socket_opts [:binary, packet: 4, active: :once]
  
  def start_link(pid, ip, port) do
    case :gen_tcp.connect(ip, port, @socket_opts) do
      {:ok, socket} ->
	{:ok, client} = GenServer.start_link(__MODULE__, [pid, ip, port, socket], [])
	:ok = :gen_tcp.controlling_process(socket, client)
	{:ok, client}
      {:error, error} ->
	{:error, error}
    end
  end

  def send_packet(pid, packet = %P2PPacket{}) do
    GenServer.cast(pid, packet)
  end
  
  def version(pid) do
    GenServer.cast(pid, %P2PPacket{proc: :version, extra_data: MBC.version})
  end
  
  def pong(pid) do
    GenServer.cast(pid, %P2PPacket{proc: :pong})
  end

  def getaddrs(pid) do
    GenServer.cast(pid, %P2PPacket{proc: :getaddrs})
  end

  def addr(pid) do
    ip = Application.get_env(:mbc, :ip)
    port = Application.get_env(:mbc, :port)
    GenServer.cast(pid, %P2PPacket{proc: :addr, extra_data: [%P2PAddr{ip: ip, port: port}]})
  end
  
  def init([pid, ip, port, socket]) do
    :ok = :inet.setopts(socket, @socket_opts)
    {:ok, %{@initial_state | socket: socket, ip: ip, port: port, pid: pid}}
  end

  def handle_cast(packet = %P2PPacket{}, state = %{socket: socket}) do
    Logger.info "Sending #{inspect(packet)}"
    payload = P2PPacket.encode(packet)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}    
  end

  def handle_info({:tcp, socket, data}, state = %{socket: socket}) do
    response = P2PPacket.decode(data)
    send state.pid, response
    :ok = :inet.setopts(socket, @socket_opts)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :normal, state}
  end
    
end

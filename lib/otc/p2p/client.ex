require Logger

defmodule OTC.P2P.Client do
  use GenServer

  @initial_state %{
    ip: nil,
    port: nil,
    socket: nil,
    pid: nil
  }

  @socket_opts [:binary, packet: 4]
  
  def start_link(pid, ip, port) do
    case :gen_tcp.connect(:erlang.binary_to_list(ip), port, @socket_opts) do
      {:ok, socket} ->
	{:ok, client} = GenServer.start_link(__MODULE__, [pid, ip, port, socket], [])
	:ok = :gen_tcp.controlling_process(socket, client)
	{:ok, client}
      {:error, error} ->
	{:error, error}
    end
  end

  def send_packet(pid, packet = %OTC.P2P.Packet{}) do
    GenServer.cast(pid, packet)
  end
  
  def version(pid) do
    GenServer.cast(pid, %OTC.P2P.Packet{proc: :version, extra_data: OTC.version})
  end
  
  def ping(pid) do
    GenServer.cast(pid, %OTC.P2P.Packet{proc: :ping})
  end

  def getaddrs(pid) do
    GenServer.cast(pid, %OTC.P2P.Packet{proc: :getaddrs})
  end

  def addr(pid) do
    ip = Application.get_env(:otc, :ip)
    port = Application.get_env(:otc, :port)
    GenServer.cast(pid, %OTC.P2P.Packet{proc: :addr, extra_data: [%OTC.P2P.Addr{ip: ip, port: port}]})
  end
  
  def init([pid, ip, port, socket]) do
    :inet.setopts(socket, [{:active, true}|@socket_opts])
    {:ok, %{@initial_state | socket: socket, ip: ip, port: port, pid: pid}}
  end

  def handle_cast(packet = %OTC.P2P.Packet{}, state = %{socket: socket}) do
    Logger.info "Sending #{inspect(packet)}"
    payload = OTC.P2P.Packet.encode(packet)
    :ok = :gen_tcp.send(socket, payload)
    {:noreply, state}    
  end

  def handle_info({:tcp, socket, data}, state = %{socket: socket}) do
    response = OTC.P2P.Packet.decode(data)
    send state.pid, response
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :normal, state}
  end
    
end

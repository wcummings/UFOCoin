require Logger

defmodule OTC.P2P.Client do
  use GenServer

  @initial_state %{
    host: nil,
    port: nil,
    socket: nil,
    pid: nil
  }
  
  def start_link(pid, host, port) do
    GenServer.start_link(__MODULE__, [pid, host, port], [])
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
  
  def init([pid, host, port]) do
    # FIXME: implement retries here
    {:ok, socket} = :gen_tcp.connect(:erlang.binary_to_list(host), port, [:binary, active: true, packet: 4])
    {:ok, %{@initial_state | socket: socket, host: host, port: port, pid: pid}}
  end

  def handle_cast(packet = %OTC.P2P.Packet{}, state = %{socket: socket}) do
    Logger.info "Sending #{packet.proc}"
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

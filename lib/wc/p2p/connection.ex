require Logger

alias WC.Blockchain.BlockValidatorServer, as: BlockValidatorServer
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.AddrTable, as: P2PAddrTable
alias WC.P2P.PingFSM, as: P2PPingFSM
alias WC.P2P.PingFSMSupervisor, as: P2PPingFSMSupervisor
alias WC.Blockchain.LogServer, as: LogServer

defmodule WC.P2P.Connection do
  use GenServer

  @initial_state %{socket: nil, pingfsm: nil}

  def child_spec(_opts) do
    %{
      id: __MODULE__,
      restart: :temporary,
      shutdown: 5000,
      start: {__MODULE__, :start_link, []},
      type: :worker
    }
  end

  def start_link(socket) do
    GenServer.start_link(__MODULE__, [socket], [])
  end

  @spec send_packet(pid(), P2PPacket.t) :: :ok
  def send_packet(pid, packet = %P2PPacket{}) when is_pid(pid) do
    GenServer.cast(pid, {:send_packet, packet})
  end

  def send_packet(socket, packet = %P2PPacket{}) do
    :ok = :gen_tcp.send(socket, P2PPacket.encode(packet))
  end

  @spec broadcast(P2PPacket.t) :: :ok
  def broadcast(packet) do
    broadcast(packet, [])
  end
  
  def broadcast(packet, excluded_pids) do
    Registry.dispatch(:connection_registry, "connection", fn entries ->
      Enum.filter(entries, fn {pid, _} -> not :lists.member(pid, excluded_pids) end)
      |> Enum.each(fn {pid, _} -> send_packet(pid, packet) end)
    end)
  end
  
  def init([socket]) do
    :ok = :inet.setopts(socket, [{:active, :once}]) # Re-set {:active, :once}    
    {:ok, _} = Registry.register(:connection_registry, "connection", [])
    {:ok, pid} = P2PPingFSMSupervisor.start_child(self())
    :true = Process.link(pid)
    {:ok, %{@initial_state | socket: socket, pingfsm: pid}}
  end

  def handle_cast({:send_packet, packet}, state = %{socket: socket}) do
    send_packet(socket, packet)
    {:noreply, state}
  end
  
  def handle_info({:tcp, socket, data}, state = %{socket: socket}) do
    packet = P2PPacket.decode(data)
    state = handle_packet(packet, state)
    :ok = :inet.setopts(socket, [{:active, :once}]) # Re-set {:active, :once}
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :disconnected, state}
  end

  def handle_packet(%P2PPacket{proc: :getaddrs}, state = %{socket: socket}) do
    addrs = P2PAddrTable.get_all
    send_packet(socket, %P2PPacket{proc: :addr, extra_data: addrs})
    state
  end

  def handle_packet(packet = %P2PPacket{proc: :addr, extra_data: addrs}, state) do
    {ip, port} = {Application.get_env(:wc, :ip), Application.get_env(:wc, :port)}

    # If theres only one addr, its prolly a node advertising and the node should broadcast it
    if length(addrs) == 1 do
      [addr] = addrs
      case P2PAddrTable.get(addr) do
	{:error, :notfound} ->
	  broadcast(packet)
	{:ok, addr_with_last_seen} ->
	  if (addr_with_last_seen.last_seen + 60 * 60 * 1000 < :os.system_time(:millisecond)) do
	    broadcast(packet)
	  end
      end
    end
    
    Enum.filter(addrs, fn addr -> {addr.ip, addr.port} != {ip, port} end)
    |> P2PAddrTable.insert

    state
  end

  def handle_packet(packet = %P2PPacket{proc: :block, extra_data: block}, state) do
    case BlockValidatorServer.validate_block(block) do
      :ok ->
	Logger.info "Block accepted: #{inspect(block)}"
	broadcast(packet)
      {:error, :alreadyaccepted} ->
	:ok # Ignore it
      {:error, error} ->
	Logger.warn "Block rejected, reason: #{inspect(error)}, block: #{inspect(block)}"
    end
    state
  end

  def handle_packet(%P2PPacket{proc: :ping}, state = %{socket: socket}) do
    send_packet(socket, %P2PPacket{proc: :ping})
    state
  end
  
  def handle_packet(%P2PPacket{proc: :pong}, state = %{pingfsm: pingfsm}) do
    P2PPingFSM.pong(pingfsm)
    state
  end

  def handle_packet(%P2PPacket{proc: :getblocks, extra_data: block_locator}, state) do
    # TODO
    state
  end
  
  def handle_packet(packet, state) do
    Logger.warn "Malformed packet: #{inspect(packet)}"
    state
  end

end

require Logger

alias MBC.Blockchain.LogServer, as: LogServer
alias MBC.Blockchain.Block, as: Block
alias MBC.P2P.Packet, as: P2PPacket
alias MBC.P2P.AddrTable, as: P2PAddrTable
alias MBC.P2P.PingFSM, as: P2PPingFSM

defmodule MBC.P2P.Connection do
  use GenServer

  @initial_state %{socket: nil}

  def child_spec(_opts) do
    %{
      id: __MODULE__,
      restart: :transient,
      shutdown: 5000,
      start: {__MODULE__, :start_link, []},
      type: :worker
    }
  end

  def start_link(socket) do
    GenServer.start_link(__MODULE__, [socket], [])
  end

  def send_packet(pid, packet = %P2PPacket{}) when is_pid(pid) do
    GenServer.cast(pid, {:send_packet, packet})
  end

  def init([socket]) do
    :ok = :inet.setopts(socket, [{:active, :once}]) # Re-set {:active, :once}    
    {:ok, _} = Registry.register(:connection_registry, "connection", [])
    {:ok, %{@initial_state | socket: socket}}
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
    {:stop, :normal, state}
  end

  def handle_packet(%P2PPacket{proc: :getaddrs}, state = %{socket: socket}) do
    addrs = P2PAddrTable.get_all
    send_packet(socket, %P2PPacket{proc: :addr, extra_data: addrs})
    state
  end

  def handle_packet(packet = %P2PPacket{proc: :addr, extra_data: addrs}, state) do
    {ip, port} = {Application.get_env(:mbc, :ip), Application.get_env(:mbc, :port)}

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
    case LogServer.get_block_by_hash(Block.hash(block)) do
      {:ok, _block} ->
	state
      {:error, :notfound} -> 
	case Block.validate_block(block) do
	  :ok ->
	    # Broadcast block if we haven't seen it before
	    LogServer.update(block)
	    broadcast(packet)
	  {:error, error} ->
	    Logger.warn "Block rejected, reason: #{inspect(error)}, block: #{inspect(block)}"
	end
	state
    end
  end

  def handle_packet(%P2PPacket{proc: :pong}, state) do
    # P2PPingFSM.pong(pingfsm)
    state
  end
  
  def handle_packet(packet, state) do
    Logger.warn "Malformed packet: #{packet}"
    state
  end

  def send_packet(socket, packet = %P2PPacket{}) do
    :ok = :gen_tcp.send(socket, P2PPacket.encode(packet))
  end
  
  def broadcast(packet) do
    Registry.dispatch(:connection_registry, "connection", fn entries ->
      for {pid, _} <- entries, do: send_packet(pid, packet)
    end)
  end
  
end

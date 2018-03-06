require Logger

alias WC.Blockchain.BlockValidatorServer, as: BlockValidatorServer
alias WC.Blockchain.InvItem, as: InvItem
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.Block, as: Block
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.AddrTable, as: P2PAddrTable
alias WC.P2P.PingFSM, as: P2PPingFSM
alias WC.P2P.PingFSMSupervisor, as: P2PPingFSMSupervisor
alias WC.Util.PriorityQueue, as: PriorityQueue

defmodule WC.P2P.Connection do
  use GenServer

  @initial_state %{socket: nil,
		   pingfsm: nil,
		   asked_for: PriorityQueue.new,
		   already_asked_for: %{},
		   last_block_hash_in_batch: nil}

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

  def handle_packet(%P2PPacket{proc: :block, extra_data: block}, state) do
    block_hash = Block.hash(block)
    case BlockValidatorServer.validate_block(block) do
      :ok ->
	Logger.info "Block accepted: #{BlockHeader.pprint(block.header)}"
	broadcast(%P2PPacket{proc: :inv, extra_data: [InvItem.from_block_hash(block_hash)]})
	handle_end_of_batch(block_hash, state)
      {:error, :alreadyaccepted} ->
	handle_end_of_batch(block_hash, state)	
      {:error, error} ->
	Logger.warn "Block rejected, reason: #{inspect(error)}, #{BlockHeader.pprint(block.header)}"
	state
    end
  end

  def handle_packet(%P2PPacket{proc: :ping}, state = %{socket: socket}) do
    send_packet(socket, %P2PPacket{proc: :ping})
    state
  end
  
  def handle_packet(%P2PPacket{proc: :pong}, state = %{pingfsm: pingfsm}) do
    P2PPingFSM.pong(pingfsm)
    state
  end

  def handle_packet(%P2PPacket{proc: :getblocks, extra_data: block_locator}, state = %{socket: socket}) do
    # Logger.info "Received block locator"
    # for hash <- block_locator, do: Logger.info "Locator hash: #{Base.encode16(hash)}"
    case LogServer.find_first_block_hash_in_longest_chain(block_locator) do
      {:error, :notfound} ->
	# TODO: do we tell the node we can't find it?
  	:ok
      {:ok, block_hash} ->
	# TODO: use constant instead of 500
	case LogServer.get_next_block_hashes_in_chain(500, block_hash) do
	  [] ->
	    # Do nothing
	    nil
	  block_hashes ->
	    # Send inventory
	    invitems = Enum.map(block_hashes, &InvItem.from_block_hash/1)
	    packet = %P2PPacket{proc: :inv, extra_data: invitems}
	    send_packet(socket, packet)
	end
    end
    state
  end

  def handle_packet(%P2PPacket{proc: :inv, extra_data: invitems}, state = %{socket: socket,
									    asked_for: asked_for,
									    already_asked_for: already_asked_for}) do
    start_time = :os.system_time(:millisecond)
    {asked_for2, new_already_asked_for} = Enum.reduce(invitems, {asked_for, already_asked_for}, &ask_for/2)
    current_ts = :os.system_time(:millisecond)
    invitems = PriorityQueue.get(asked_for2, current_ts)
    |> Enum.filter(fn invitem -> LogServer.exists?(invitem.hash) end)
    |> Enum.take(1) # FIXME: make configurable or something
    asked_for3 = Enum.reduce(invitems, asked_for2, &PriorityQueue.delete/2)
    end_time = :os.system_time(:millisecond)
    # Logger.debug "Built getdata batches in #{end_time - start_time}ms"
    if length(invitems) > 0 do
      send_packet(socket, %P2PPacket{proc: :getdata, extra_data: invitems})
      %InvItem{type: :block, hash: last_block_hash_in_batch} = Enum.at(invitems, -1)
      %{state | last_block_hash_in_batch: last_block_hash_in_batch, asked_for: asked_for3, already_asked_for: new_already_asked_for}
    else
      %{state | already_asked_for: new_already_asked_for}
    end
  end

  def handle_packet(%P2PPacket{proc: :getdata, extra_data: invitems}, state = %{socket: socket}) do
    for invitem <- invitems do
      case LogServer.get_block_by_hash(invitem.hash) do
	{:error, :notfound} ->
	  nil
	{:ok, block} ->
	  send_packet(socket, %P2PPacket{proc: :block, extra_data: block})
      end
    end
    state
  end
  
  def handle_packet(packet, state) do
    Logger.warn "Malformed packet: #{inspect(packet)}"
    state
  end

  def ask_for(invitem, {asked_for, already_asked_for}) do
    if PriorityQueue.member?(asked_for, invitem) or LogServer.exists?(invitem) do
      # Do nothing if the item is already in the queue
      {asked_for, already_asked_for}
    else
      current_ts = :os.system_time(:millisecond)
      request_time = WC.Util.max((already_asked_for[invitem.hash] || 0) + 2 * 60 * 1000, current_ts)
      {PriorityQueue.insert(asked_for, request_time, invitem),
       Map.put(already_asked_for, invitem.hash, request_time)}
    end
  end

  def handle_end_of_batch(block_hash, state = %{last_block_hash_in_batch: block_hash, socket: socket}) do
    # FIXME: send block locator
    send_packet(socket, %P2PPacket{proc: :getblocks, extra_data: [InvItem.from_block_hash(block_hash)]})
    %{state | last_block_hash_in_batch: nil}
  end

  def handle_end_of_batch(_, state) do
    state
  end
  
end

require Logger

alias WC.Blockchain.BlockValidatorServer, as: BlockValidatorServer
alias WC.Blockchain.InvItem, as: InvItem
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.Block, as: Block
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.AddrTable, as: P2PAddrTable
alias WC.P2P.Addr, as: P2PAddr
alias WC.P2P.PingFSM, as: P2PPingFSM
alias WC.P2P.PingFSMSupervisor, as: P2PPingFSMSupervisor
alias WC.P2P.ConnectionRegistry, as: P2PConnectionRegistry
alias WC.Util.PriorityQueue, as: PriorityQueue

defmodule WC.P2P.Connection do
  use GenServer

  @initial_state %{socket: nil,
		   pingfsm: nil,
		   asked_for: PriorityQueue.new,
		   already_asked_for: %{},
		   last_block_hash_in_batch: nil}

  @block_batch_size 500
  
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
    # Logger.debug ">>> #{inspect(packet)}"
    :ok = :gen_tcp.send(socket, P2PPacket.encode(packet))
  end

  def init([socket]) do
    :ok = :inet.setopts(socket, [{:active, :once}]) # Re-set {:active, :once}    
    P2PConnectionRegistry.register("new_tip")
    P2PConnectionRegistry.register("packet")
    {:ok, pid} = P2PPingFSMSupervisor.start_child(self())
    :true = Process.link(pid)
    _ref = Process.send_after(self(), :flush_asked_for, 10 * 1000)
    send self(), :send_getblocks
    Process.send_after(self(), :send_addr, 12 * 60 * 60 * 1000)
    {:ok, %{@initial_state | socket: socket, pingfsm: pid}}
  end

  def handle_cast({:send_packet, packet}, state = %{socket: socket}) do
    send_packet(socket, packet)
    {:noreply, state}
  end

  def handle_info(:send_getblocks, state = %{socket: socket}) do
    if LogServer.index_complete? do
      :ok = send_packet(socket, %P2PPacket{proc: :getblocks, extra_data: LogServer.get_block_locator()})
    end
    {:noreply, state}
  end

  def handle_info(:send_addr, state = %{socket: socket}) do
    {ip, port} = {Application.get_env(:wc, :ip), Application.get_env(:wc, :port)}
    :ok = send_packet(socket, %P2PPacket{proc: :addr, extra_data: [%P2PAddr{ip: ip, port: port}]})
    Process.send_after(self(), :send_addr, 12 * 60 * 60 * 1000)
    {:noreply, state}
  end

  def handle_info({:connection_registry, "new_tip", tip}, state = %{socket: socket}) do
    :ok = send_packet(socket, %P2PPacket{proc: :getblocks, extra_data: LogServer.get_block_locator(tip)})
    {:noreply, state}
  end

  def handle_info({:connection_registry, "packet", packet}, state = %{socket: socket}) do
    :ok = send_packet(socket, packet)
    {:noreply, state}
  end
  
  def handle_info({:tcp, socket, data}, state = %{socket: socket}) do
    packet = P2PPacket.decode(data)
    # Logger.debug "<<< #{inspect(packet)}"    
    state = handle_packet(packet, state)
    :ok = :inet.setopts(socket, [{:active, :once}]) # Re-set {:active, :once}
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    :gen_tcp.close(socket)
    {:stop, :disconnected, state}
  end

  def handle_info(:flush_asked_for, state = %{asked_for: asked_for, already_asked_for: already_asked_for, socket: socket}) do
    # Enum.filter(already_asked_for, fn {k, v} -> 
    
    # # Expire already_asked_for keys
    # new_already_asked_for = Enum.filter(already_asked_for, fn
    #   {k, v} when v < current_ts - 2 * 60 * 1000 -> false;
    #   _ -> true
    # end)
    
    current_ts = :os.system_time(:millisecond)
    invitems = PriorityQueue.get(asked_for, current_ts+1)
    |> Enum.filter(fn invitem -> not LogServer.exists?(invitem.hash) end)
    |> Enum.filter(fn invitem -> (not Map.has_key?(already_asked_for, invitem.hash)) or (already_asked_for[invitem.hash] < current_ts - 2 * 60 * 1000) end)
    |> Enum.take(@block_batch_size)
    # end_time = :os.system_time(:millisecond)
    # Logger.debug "Built getdata batches in #{end_time - start_time}ms"
    if length(invitems) > 0 do
      # Logger.debug "Sending getdata: #{inspect(Enum.map(invitems, fn invitem -> Base.encode16(invitem.hash) end))}"
      :ok = send_packet(socket, %P2PPacket{proc: :getdata, extra_data: invitems})
      new_already_asked_for = Enum.reduce(invitems, already_asked_for, fn (invitem, map) -> Map.put(map, invitem.hash, current_ts) end)
      %InvItem{type: :block, hash: last_block_hash_in_batch} = Enum.at(invitems, -1)
      _ref = Process.send_after(self(), :flush_asked_for, 10 * 1000)
      {:noreply, %{state | last_block_hash_in_batch: last_block_hash_in_batch, already_asked_for: new_already_asked_for}}      
    else
      _ref = Process.send_after(self(), :flush_asked_for, 10 * 1000)      
      {:noreply, state}
    end
  end

  def handle_packet(%P2PPacket{proc: :getaddrs}, state = %{socket: socket}) do
    addrs = P2PAddrTable.get_all
    send_packet(socket, %P2PPacket{proc: :addr, extra_data: addrs})
    state
  end

  def handle_packet(%P2PPacket{proc: :addr, extra_data: addrs}, state) do
    {ip, port} = {Application.get_env(:wc, :ip), Application.get_env(:wc, :port)}

    # If theres only one addr, its prolly a node advertising and the node should broadcast it
    if length(addrs) == 1 do
      [addr] = addrs
      case P2PAddrTable.get(addr) do
	{:error, :notfound} ->
	  P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :addr, extra_data: [addr]}, [self()])
	{:ok, addr_with_last_seen} ->
	  if (addr_with_last_seen.last_seen + 60 * 60 * 1000 < :os.system_time(:millisecond)) do
	    P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :addr, extra_data: [addr]}, [self()])	    
	  end
      end
    end
    
    Enum.filter(addrs, fn addr -> {addr.ip, addr.port} != {ip, port} end)
    |> P2PAddrTable.insert

    state
  end

  def handle_packet(%P2PPacket{proc: :block, extra_data: block}, state = %{asked_for: asked_for}) do
    block_hash = Block.hash(block)
    :ok = BlockValidatorServer.validate_block(block)
    handle_end_of_batch(block_hash, state)
    %{state | asked_for: PriorityQueue.delete(InvItem.from_block_hash(block_hash), asked_for)}
  end

  def handle_packet(%P2PPacket{proc: :ping}, state = %{socket: socket}) do
    send_packet(socket, %P2PPacket{proc: :pong})
    state
  end
  
  def handle_packet(%P2PPacket{proc: :pong}, state = %{pingfsm: pingfsm}) do
    :ok = P2PPingFSM.pong(pingfsm)
    state
  end

  def handle_packet(%P2PPacket{proc: :getblocks, extra_data: block_locator}, state = %{socket: socket}) do
    case LogServer.find_first_block_hash_in_longest_chain(block_locator) do
      {:error, :index_incomplete} ->
	:ok # IGNORE
      {:error, :notfound} ->
	# TODO: do we tell the node we can't find it?
  	:ok
      {:ok, block_hash} ->
	case LogServer.get_next_block_hashes_in_chain(@block_batch_size, block_hash) do
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

  def handle_packet(%P2PPacket{proc: :inv, extra_data: invitems}, state = %{asked_for: asked_for}) do
    new_asked_for = Enum.reduce(invitems, asked_for, &ask_for/2)
    send self(), :flush_asked_for
    %{state | asked_for: new_asked_for}
  end

  def handle_packet(%P2PPacket{proc: :getdata, extra_data: invitems}, state = %{socket: socket}) do
    # Logger.info "Received getdata request: #{inspect(Enum.map(invitems, fn invitem -> Base.encode16(invitem.hash) end))}"
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

  def ask_for(invitem, asked_for) do
    if PriorityQueue.member?(asked_for, invitem) or LogServer.exists?(invitem) do
      # Do nothing if the item is already in the queue
      asked_for
    else
      PriorityQueue.insert(asked_for, :os.system_time(:millisecond), invitem)
    end
  end

  def handle_end_of_batch(block_hash, state = %{last_block_hash_in_batch: block_hash, socket: socket}) do
    # :ok = send_packet(socket, %P2PPacket{proc: :getblocks, extra_data: [block_hash]})
    :ok = send_packet(socket, %P2PPacket{proc: :getblocks, extra_data: LogServer.get_block_locator()})
    %{state | last_block_hash_in_batch: nil}
  end

  def handle_end_of_batch(_, state) do
    state
  end
  
end

require Logger

alias OTC.P2P.Client, as: P2PClient
alias OTC.P2P.AddrServer, as: P2PAddrServer
alias OTC.P2P.Addr, as: P2PAddr
alias OTC.P2P.Packet, as: P2PPacket
alias OTC.P2P.AddrTable, as: P2PAddrTable

defmodule OTC.P2P.ClientFSM do
  @behaviour :gen_statem

  @initial_state %{client: nil, ip: nil, port: nil, retries: 0}
  @handshake_timeout_ms 10000

  def start_link(opts) do
    :gen_statem.start_link(__MODULE__, [], opts)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      restart: :permanent,
      shutdown: 5000,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker
    }
  end

  def init([]) do
    send self(), :checkout
    {:ok, :checkout_addr, @initial_state}
  end

  def checkout_addr(:info, :checkout, data) do
    case P2PAddrServer.checkout do
      {:ok, %P2PAddr{ip: ip, port: port}} ->
	Logger.info "Connecting... #{ip}:#{port}"
	{:next_state, :connecting, %{data | ip: ip, port: port}, 0}
      # Throttle retries when the node has not discovered enough peers yet
      {:error, :exhausted} ->
	Logger.info "Not enough peers in database, waiting 10s before retrying..."
	Process.send_after(self(), :checkout, 10 * 1000)
	{:keep_state, data}
    end
  end

  def connecting(:timeout, _, data = %{ip: ip, port: port}) do
    case P2PClient.start_link(self(), ip, port) do
      {:ok, client} ->
	{:next_state, :starting_handshake, %{data | ip: ip, port: port, client: client}, 0}
      {:error, error} ->
	Logger.info "Connection error: #{inspect(error)}"
	if data.retries > 3 do # TODO: make this configurable
	  {:next_state, :checkout_addr, @initial_state, 0}
	else
	  {:next_state, :connecting, %{data | retries: data.retries + 1}, 10 * 1000}
	end
    end
  end
  
  def starting_handshake(:timeout, _, data = %{client: client}) do
    P2PClient.version(client)
    {:next_state, :waiting_for_handshake, data, @handshake_timeout_ms}
  end

  def waiting_for_handshake(:info, %P2PPacket{proc: :version, extra_data: version_string}, data) do
    if OTC.version != version_string do
      {:stop, :normal}
    else
      {:keep_state, data}
    end
  end

  def waiting_for_handshake(:info, %P2PPacket{proc: :versionack}, data = %{client: client}) do
    P2PClient.getaddrs(client)
    P2PClient.addr(client)
    Logger.info "Connected #{data.ip}:#{data.port}"
    {:next_state, :connected, data}
  end

  def waiting_for_handshake(:timeout, _, _data) do
    Logger.info "Timeout waiting for handshake"
    {:stop, :normal}
  end
  
  def connected(:info, packet = %P2PPacket{proc: :addr, extra_data: addrs}, data) do
    # If theres only one addr, its prolly a node advertising and the node should broadcast it
    if length(addrs) == 1 do
      [addr] = addrs
      addr_with_last_seen = P2PAddrTable.get_addr(addr)
      if (addr_with_last_seen.last_seen + 10 * 1000 < :os.system_time(:millisecond)) do
	P2PAddrServer.broadcast(packet)
      end
    end
    P2PAddrTable.add_addrs(addrs)
    {:keep_state, data}
  end

  def connected(:cast, {:broadcast, packet = %P2PPacket{}}, %{client: client}) do
    P2PClient.send_packet(client, packet)
    :keep_state_and_data
  end
  
  def callback_mode, do: :state_functions

  def handle_event({:call, from}, _event, data), do: {:keep_state, data, [{:reply, from, {:error, :undef}}]}
  def handle_event(_event, data), do: {:keep_state, data}
  def terminate(_reason, _state, _data), do: :void
  def code_change(_vsn, state, data, _extra), do: {:ok, state, data}

end

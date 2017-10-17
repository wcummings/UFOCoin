require Logger

defmodule OTC.P2P.ClientFSM do
  @behaviour :gen_statem

  @initial_state %{client: nil, ip: nil, port: nil}
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
    case OTC.P2P.AddrServer.checkout do
      {:ok, %OTC.P2P.Addr{ip: ip, port: port}} ->
	Logger.info "Connecting... #{ip}:#{port}"
	{:ok, client} = OTC.P2P.Client.start_link(self(), ip, port)
	send self(), :start_handshake
	{:next_state, :starting_handshake, %{data | ip: ip, port: port, client: client}}
      # Throttle retries when the node has not discovered enough peers yet
      {:error, :exhausted} ->
	Logger.info "Not enough peers in database, waiting 10s before retrying..."
	Process.send_after(self(), :checkout, 10 * 1000, [])
	{:keep_state, data}
    end
  end
  
  def starting_handshake(:info, :start_handshake, data = %{client: client}) do
    OTC.P2P.Client.version(client)
    {:next_state, :waiting_for_handshake, data, @handshake_timeout_ms}
  end

  def waiting_for_handshake(:info, %OTC.P2P.Packet{proc: :version, extra_data: version_string}, data) do
    if OTC.version != version_string do
      {:stop, :normal}
    else
      {:keep_state, data}
    end
  end

  def waiting_for_handshake(:info, %OTC.P2P.Packet{proc: :versionack}, data = %{client: client}) do
    OTC.P2P.Client.getaddrs(client)
    OTC.P2P.Client.addr(client)
    Logger.info "Connected #{data.ip}:#{data.port}"
    {:next_state, :connected, data}
  end

  def waiting_for_handshake(:timeout, _, _data) do
    {:stop, :normal}
  end
  
  def connected(:info, packet = %OTC.P2P.Packet{proc: :addr, extra_data: addrs}, data) do
    # If theres only one addr, its prolly a node advertising and the node should broadcast it
    if length(addrs) == 1 do
      [addr] = addrs
      addr_with_last_seen = OTC.P2P.AddrTable.get_addr(addr)
      if (addr_with_last_seen.last_seen + 10 * 1000 < :os.system_time(:millisecond)) do
	OTC.P2P.AddrServer.broadcast(packet)
      end
    end
    OTC.P2P.AddrTable.add_addrs(addrs)
    {:keep_state, data}
  end
  
  def callback_mode, do: :state_functions

  def handle_event({:call, from}, _event, data), do: {:keep_state, data, [{:reply, from, {:error, :undef}}]}
  def handle_event(_event, data), do: {:keep_state, data}
  def terminate(_reason, _state, _data), do: :void
  def code_change(_vsn, state, data, _extra), do: {:ok, state, data}
  
end

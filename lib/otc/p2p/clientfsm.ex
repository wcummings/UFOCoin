require Logger

defmodule OTC.P2P.ClientFSM do
  @behaviour :gen_statem

  @initial_state %{client: nil}
  @handshake_timeout_ms 10000

  def start_link(opts) do
    case OTC.P2P.AddrServer.checkout() do
      {:ok, %OTC.P2P.Addr{ip: ip, port: port}} ->
	start_link(ip, port)
      # Throttle retries when the node has not discovered enough peers yet
      {:error, :exhausted} ->
	  Logger.info "Not enough peers in database, waiting 10s before retrying..."
	  :timer.sleep(10 * 1000)
	  start_link(opts)
    end
  end
  
  def start_link(host, port) do
    :gen_statem.start_link(__MODULE__, [host, port], [])
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      restart: :permanent,
      shutdown: 5000,
      start: {__MODULE__, :start_link, [[]]},
      type: :worker
    }
  end

  def init([host, port]) do
    send self(), :start_handshake
    {:ok, client} = OTC.P2P.Client.start_link(self(), host, port)
    {:ok, :starting_handshake, %{@initial_state | client: client}}
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
    {:next_state, :connected, data}
  end

  def waiting_for_handshake(:state_timeout, _, _data) do
    {:stop, :normal}
  end
  
  def connected(:info, %OTC.P2P.Packet{proc: :addr, extra_data: addrs}, data) do
    OTC.P2P.AddrTable.add_addrs(addrs)
    {:keep_state, data}
  end
  
  def callback_mode, do: :state_functions

  def handle_event({:call, from}, _event, data), do: {:keep_state, data, [{:reply, from, {:error, :undef}}]}
  def handle_event(_event, data), do: {:keep_state, data}
  def terminate(_reason, _state, _data), do: :void
  def code_change(_vsn, state, data, _extra), do: {:ok, state, data}
  
end

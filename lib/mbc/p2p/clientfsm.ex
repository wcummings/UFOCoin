require Logger

alias MBC.P2P.AddrServer, as: P2PAddrServer
alias MBC.P2P.Addr, as: P2PAddr
alias MBC.P2P.Packet, as: P2PPacket
alias MBC.P2P.ConnectionSupervisor, as: P2PConnectionSupervisor

defmodule MBC.P2P.ClientFSM do
  @behaviour :gen_statem

  @initial_state %{
    socket: nil,
    ip: nil,
    port: nil,
    retries: 0,
    connection: nil
  }
  
  @handshake_timeout_ms 10000

  @socket_opts [:binary, packet: 4, active: :once]
  
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

  def handle_event(:info, :checkout, :checkout_addr, data) do
    case P2PAddrServer.checkout do
      {:ok, %P2PAddr{ip: ip, port: port}} ->
	{:next_state, :connecting, %{data | ip: ip, port: port}, 0}
      {:error, :exhausted} ->
	Logger.info "Not enough peers in database, waiting 10s before retrying..."
	Process.send_after(self(), :checkout, 10 * 1000)
	{:keep_state, data}
    end
  end

  def handle_event(:timeout, _, :connecting, data = %{ip: ip, port: port}) do
    case :gen_tcp.connect(ip, port, @socket_opts) do
      {:ok, socket} ->
	{:next_state, :starting_handshake, %{data | ip: ip, port: port, socket: socket}, 0}
      {:error, error} ->
	Logger.info "Connection error: #{inspect(error)}"
	if data.retries > 3 do # TODO: make this configurable
	  {:next_state, :checkout_addr, @initial_state, 0}
	else
	  {:next_state, :connecting, %{data | retries: data.retries + 1}, 10 * 1000}
	end
    end
  end
  
  def handle_event(:timeout, _, :starting_handshake, data = %{socket: socket}) do  
    send_packet(socket, %P2PPacket{proc: :version, extra_data: MBC.version})
    {:next_state, :waiting_for_handshake, data, @handshake_timeout_ms}
  end

  def handle_event(:info, %P2PPacket{proc: :version, extra_data: version_string}, :waiting_for_handshake, data) do
    if MBC.version != version_string do
      Logger.info("Version mismatch #{MBC.version} != #{version_string}")
      {:stop, :normal}
    else
      {:keep_state, data}
    end
  end

  def handle_event(:info, %P2PPacket{proc: :versionack}, :waiting_for_handshake, data = %{socket: socket}) do
    send_packet(socket, %P2PPacket{proc: :getaddrs})
    {ip, port} = {Application.get_env(:mbc, :ip), Application.get_env(:mbc, :port)}
    send_packet(socket, %P2PPacket{proc: :addr, extra_data: [%P2PAddr{ip: ip, port: port}]})
    {:ok, pid} = P2PConnectionSupervisor.new_connection(socket)
    :true = Process.link(pid)
    :ok = :gen_tcp.controlling_process(socket, pid)
    {:next_state, :connected, %{data | connection: pid}}
  end
  
  def handle_event(:timeout, _, :waiting_for_handshake, _data) do
    Logger.info "Handshake timeout"
    {:stop, :normal}
  end

  def handle_event(:info, {:tcp, _socket, tcpdata}, _, %{socket: socket}) do
    packet = P2PPacket.decode(tcpdata)
    send self(), packet
    :ok = :inet.setopts(socket, @socket_opts)
    :keep_state_and_data
  end

  def send_packet(socket, packet) do
    :ok = :gen_tcp.send(socket, P2PPacket.encode(packet))
  end

  def callback_mode, do: :handle_event_function

  def handle_event({:call, from}, _event, data), do: {:keep_state, data, [{:reply, from, {:error, :undef}}]}
  def handle_event(_event, _content, data), do: {:keep_state, data}  
  def terminate(_reason, _state, _data), do: :void
  def code_change(_vsn, state, data, _extra), do: {:ok, state, data}

end

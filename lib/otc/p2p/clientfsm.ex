require Logger

defmodule OTC.P2P.ClientFSM do
  @behaviour :gen_statem

  @initial_state %{client: nil}
  @handshake_timeout_ms 10000
  
  def start_link(host, port) do
    :gen_statem.start_link(__MODULE__, [host, port], [])
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

  def waiting_for_handshake(:info, %OTC.P2P.Packet{proc: :version, extra_data: %{"vsn" => version}}, data) do
    if OTC.version != version do
      {:stop, :normal}
    else
      {:keep_state, data}
    end
  end

  def waiting_for_handshake(:info, %OTC.P2P.Packet{proc: :versionack}, data = %{client: client}) do
    OTC.P2P.Client.getaddrs(client)
    # OTC.P2PClient.addr(client)
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

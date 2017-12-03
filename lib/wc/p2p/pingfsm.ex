alias WC.P2P.Connection, as: P2PConnection
alias WC.P2P.Packet, as: P2PPacket

defmodule WC.P2P.PingFSM do
  @behaviour :gen_statem

  @ping_timeout 60 * 60 * 1000

  def child_spec(_opts) do
    %{
      id: __MODULE__,
      restart: :temporary,
      shutdown: 5000,
      start: {__MODULE__, :start_link, []},
      type: :worker
    }
  end
  
  def start_link(pid) do
    :gen_statem.start_link(__MODULE__, [pid], [])
  end

  def pong(pid) do
    :gen_statem.cast(pid, :pong)
  end
  
  def init([pid]) do
    {:ok, :sending, %{pid: pid}, 0}
  end

  def sending(:cast, :pong, _data) do
    {:keep_state_and_data, @ping_timeout}
  end

  def sending(:timeout, _, data = %{pid: pid}) do
    :ok = P2PConnection.send_packet(pid, %P2PPacket{proc: :ping})
    {:next_state, :waiting, data, @ping_timeout}
  end

  def waiting(:cast, :pong, data) do
    {:next_state, :sending, data, @ping_timeout}
  end

  def waiting(:timeout, _, _) do
    exit(:timeout)
  end

  def callback_mode, do: :state_functions

  def handle_event({:call, from}, _event, data), do: {:keep_state, data, [{:reply, from, {:error, :undef}}]}
  def handle_event(_event, data), do: {:keep_state, data}
  def terminate(_reason, _state, _data), do: :void
  def code_change(_vsn, state, data, _extra), do: {:ok, state, data}
  
end

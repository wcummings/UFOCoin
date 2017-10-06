defmodule OBC.P2PServer do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, %{peers: []}, [])
  end

  def init(state) do
    {:ok, state}
  end

  def handle_call(:get_peers, state) do
    {:reply, state.peers}
  end

end

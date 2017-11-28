defmodule MBC.P2P.HandshakeSupervisor do
  use Supervisor

  @name MBC.P2P.HandshakeSupervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.P2P.Handshake], strategy: :simple_one_for_one)
  end

  def start_child(socket) do
    Supervisor.start_child(@name, [socket])
  end
  
end

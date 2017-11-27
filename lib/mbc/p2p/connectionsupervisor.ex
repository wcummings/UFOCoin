defmodule MBC.P2P.ConnectionSupervisor do
  use Supervisor

  @name MBC.P2P.ConnectionSupervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.P2P.Connection], strategy: :simple_one_for_one)
  end

  def new_connection(socket) do
    Supervisor.start_child(@name, [socket])
  end
  
end

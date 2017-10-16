defmodule OTC.P2P.ClientFSMSupervisor do
  use Supervisor

  @name OTC.P2P.ClientFSMSupervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    outbound_connections = Application.get_env(OTC, :outbound_connections)
    children = for _ <- 1 .. outbound_connections, do: OTC.P2P.ClientFSM

    Supervisor.init([OTC.P2P.ClientFSM], strategy: :simple_one_for_one)
  end

  def start_client do
    Supervisor.start_child(@name, [])
  end
  
end

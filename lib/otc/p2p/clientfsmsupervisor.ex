defmodule OTC.P2P.ClientFSMSupervisor do
  use Supervisor

  @name OTC.P2P.ClientFSMSupervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([OTC.P2P.ClientFSM], strategy: :simple_one_for_one)
  end

  def start_client do
    Supervisor.start_child(@name, [])
  end
  
end

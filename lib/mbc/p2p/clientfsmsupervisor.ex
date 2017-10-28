defmodule MBC.P2P.ClientFSMSupervisor do
  use Supervisor

  @name MBC.P2P.ClientFSMSupervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.P2P.ClientFSM], strategy: :simple_one_for_one)
  end

  def start_client do
    Supervisor.start_child(@name, [])
  end
  
end

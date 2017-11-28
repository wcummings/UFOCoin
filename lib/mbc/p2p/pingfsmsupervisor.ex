defmodule MBC.P2P.PingFSMSupervisor do
  use Supervisor

  @name MBC.P2P.PingFSMSupervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.P2P.PingFSM], strategy: :simple_one_for_one)
  end

  def start_child(pid) do
    Supervisor.start_child(@name, [pid])
  end
  
end

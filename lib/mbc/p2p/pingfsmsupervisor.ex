defmodule MBC.P2P.PingFSMSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Supervisor.init([MBC.P2P.PingFSM], strategy: :simple_one_for_one)
  end

  def start_child(pid) do
    Supervisor.start_child(__MODULE__, [pid])
  end
  
end

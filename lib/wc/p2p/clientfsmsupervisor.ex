defmodule WC.P2P.ClientFSMSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Supervisor.init([WC.P2P.ClientFSM], strategy: :simple_one_for_one)
  end

  def start_client do
    Supervisor.start_child(__MODULE__, [])
  end
  
end

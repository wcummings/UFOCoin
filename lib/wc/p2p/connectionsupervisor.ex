defmodule WC.P2P.ConnectionSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Supervisor.init([WC.P2P.Connection], strategy: :simple_one_for_one)
  end

  def new_connection(socket) do
    Supervisor.start_child(__MODULE__, [socket])
  end
  
end

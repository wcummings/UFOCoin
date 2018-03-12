alias WC.P2P.ClientFSM, as: ClientFSM

defmodule WC.P2P.ClientFSMSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    number_of_connections = Application.get_env(:wc, :outbound_connections)    
    children = Enum.map(1..number_of_connections, fn n -> ClientFSM.child_spec([n]) end)
    Supervisor.init(children, strategy: :one_for_one)
  end

  def start_client do
    Supervisor.start_child(__MODULE__, [])
  end
  
end

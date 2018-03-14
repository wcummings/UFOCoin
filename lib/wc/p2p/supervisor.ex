defmodule WC.P2P.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do    
    children = [
      worker(WC.P2P.AddrServer, []),
      supervisor(WC.P2P.HandshakeSupervisor, []),
      {Registry, [keys: :duplicate, name: :connection_registry]},
      supervisor(WC.P2P.ConnectionSupervisor, []),
      supervisor(WC.P2P.PingFSMSupervisor, []),      
      supervisor(WC.P2P.ClientFSMSupervisor, [])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

defmodule WC.P2P.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    port = Application.get_env(:wc, :port)
    
    children = [
      supervisor(WC.P2P.HandshakeSupervisor, []),
      supervisor(WC.P2P.ClientFSMSupervisor, []),
      supervisor(WC.P2P.ConnectionSupervisor, []),
      supervisor(WC.P2P.PingFSMSupervisor, []),
      worker(WC.P2P.AddrServer, []),
      worker(WC.P2P.Acceptor, [port]),
      {Registry, [keys: :duplicate, name: :connection_registry]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

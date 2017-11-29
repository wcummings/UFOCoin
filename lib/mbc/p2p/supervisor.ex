defmodule MBC.P2P.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    port = Application.get_env(:mbc, :port)
    
    children = [
      supervisor(MBC.P2P.ClientFSMSupervisor, []),
      supervisor(MBC.P2P.ConnectionSupervisor, []),
      supervisor(MBC.P2P.PingFSMSupervisor, []),
      supervisor(MBC.P2P.HandshakeSupervisor, []),
      worker(MBC.P2P.AddrServer, []),
      worker(MBC.P2P.Acceptor, [port]),
      {Registry, [keys: :duplicate, name: :connection_registry]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

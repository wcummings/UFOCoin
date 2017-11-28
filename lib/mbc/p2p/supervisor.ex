defmodule MBC.P2P.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    port = Application.get_env(:mbc, :port)
    
    children = [
      {MBC.P2P.ClientFSMSupervisor, name: MBC.P2P.ClientFSMSupervisor},
      {MBC.P2P.AddrServer, name: MBC.P2P.AddrServer},      
      {MBC.P2P.ConnectionSupervisor, name: MBC.P2P.ConnectionSupervisor},
      {MBC.P2P.PingFSMSupervisor, name: MBC.P2P.PingFSMSupervisor},
      {MBC.P2P.HandshakeSupervisor, name: MBC.P2P.HandshakeSupervisor},
      worker(MBC.P2P.Acceptor, [port]),
      {Registry, [keys: :duplicate, name: :connection_registry]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

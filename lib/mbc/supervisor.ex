defmodule MBC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      {MBC.P2P.ClientFSMSupervisor, name: MBC.P2P.ClientFSMSupervisor},
      {MBC.P2P.AddrServer, name: MBC.P2P.AddrServer},
      {MBC.Miner.Supervisor, name: MBC.Miner.Supervisor},
      {MBC.Blockchain.Server, name: MBC.Blockchain.Server}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

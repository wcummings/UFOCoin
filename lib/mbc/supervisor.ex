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
      {MBC.Blockchain.LogServer, name: MBC.Blockchain.LogServer}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

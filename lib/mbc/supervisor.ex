defmodule MBC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      {MBC.Blockchain.LogServer, name: MBC.Blockchain.LogServer},
      {MBC.P2P.Supervisor, name: MBC.P2P.Supervisor},
      {MBC.Miner.Supervisor, name: MBC.Miner.Supervisor}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

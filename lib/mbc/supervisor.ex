defmodule MBC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      worker(MBC.Blockchain.LogServer, []),
      supervisor(MBC.P2P.Supervisor, []),
      supervisor(MBC.Miner.Supervisor, []),
      worker(MBC.Blockchain.BlockValidatorServer, [])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

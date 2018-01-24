defmodule WC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      worker(WC.Blockchain.LogServer, []),
      supervisor(WC.P2P.Supervisor, []),
      supervisor(WC.Miner.Supervisor, []),
      worker(WC.Blockchain.BlockValidatorServer, []),
      worker(Cachex, [:block_cache, [limit: 500]])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

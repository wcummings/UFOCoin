defmodule WC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      worker(WC.Blockchain.LogServer, []),
      supervisor(WC.Miner.Supervisor, []),
      supervisor(WC.P2P.Supervisor, []),
      worker(WC.Blockchain.BlockValidatorServer, []),
      worker(Cachex, [:block_cache, [limit: 500]]),
      worker(WC.Blockchain.InventoryServer, [])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

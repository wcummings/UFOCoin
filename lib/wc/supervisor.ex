defmodule WC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      worker(Cachex, [:block_cache, [limit: 500]]),
      {Registry, [keys: :duplicate, name: :connection_registry]},
      worker(WC.Blockchain.LogServer, []),
      supervisor(WC.P2P.Supervisor, []),
      supervisor(WC.Miner.Supervisor, [])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  
end

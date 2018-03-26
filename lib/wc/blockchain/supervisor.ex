defmodule WC.Blockchain.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      worker(WC.Blockchain.LogServer, []),
      worker(WC.Blockchain.UTXOServer, [])
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
  
end

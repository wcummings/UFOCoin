defmodule WC.Miner.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      supervisor(WC.Miner.WorkerSupervisor, []),
      worker(WC.Miner.MinerServer, [])
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

end

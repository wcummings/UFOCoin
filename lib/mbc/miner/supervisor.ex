defmodule MBC.Miner.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      worker(MBC.Miner.MinerServer, []),
      supervisor(MBC.Miner.WorkerSupervisor, [])
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

end

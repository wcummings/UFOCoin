defmodule MBC.Miner.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    children = [
      {MBC.Miner.MinerServer, name: MBC.Miner.MinerServer},
      {MBC.Miner.WorkerSupervisor, name: MBC.Miner.WorkerSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

end

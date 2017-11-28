defmodule MBC.Miner.WorkerSupervisor do
  use Supervisor

  @name MBC.Miner.WorkerSupervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.Miner.Worker], strategy: :simple_one_for_one)
  end

  def start_child(block) do
    Supervisor.start_child(@name, [block])
  end
  
end

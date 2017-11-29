defmodule MBC.Miner.WorkerSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Supervisor.init([MBC.Miner.Worker], strategy: :simple_one_for_one)
  end

  def start_child(block) do
    Supervisor.start_child(__MODULE__, [block])
  end
  
end

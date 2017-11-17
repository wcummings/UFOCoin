defmodule MBC.Mining.WorkerSupervisor do
  use Supervisor

  @name MBC.Mining.WorkerSupervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init([]) do
    Supervisor.init([MBC.Mining.Worker], strategy: :simple_one_for_one)
  end

  def start_worker(block) do
    Supervisor.start_child(@name, [block])
  end
  
end

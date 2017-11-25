alias MBC.Blockchain.Block, as: Block
alias MBC.Miner.WorkerSupervisor, as: WorkerSupervisor
alias MBC.Miner.Worker, as: MinerWorker

require Logger

defmodule MBC.Miner.MinerServer do
  use GenServer

  @initial_state %{pids: [], proc_count: nil}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def new_block(tip) do
    GenServer.cast(__MODULE__, {:new_block, tip})
  end
  
  def init([]) do
    proc_count = Application.get_env(:otc, :mining_proc_count, 1)
    {:ok, %{@initial_state | proc_count: proc_count}}
  end

  def handle_cast({:new_block, tip}, state = %{pids: pids, proc_count: proc_count}) do
    for pid <- pids, do: MinerWorker.stop(pid)
    new_block = Block.next_block(tip)
    # TODO: get tx's for new block from mempool
    new_pids = Enum.map(1 .. proc_count, fn ->
      {:ok, pid} = WorkerSupervisor.start_worker(new_block)
      pid
    end)
    {:noreply, %{state | pids: new_pids}}
  end

end

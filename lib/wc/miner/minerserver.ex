alias WC.Blockchain.Block, as: Block
alias WC.Miner.WorkerSupervisor, as: WorkerSupervisor
alias WC.Miner.Worker, as: MinerWorker

require Logger

defmodule WC.Miner.MinerServer do
  use GenServer

  @initial_state %{pids: [], proc_count: nil}
  
  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
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
    Logger.info "Mining new block #{inspect(new_block)}"    
    # TODO: get tx's for new block from mempool
    new_pids = Enum.map(1 .. proc_count, fn _ ->
      {:ok, pid} = WorkerSupervisor.start_child(new_block)
      pid
    end)
    {:noreply, %{state | pids: new_pids}}
  end

end

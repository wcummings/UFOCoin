alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Miner.WorkerSupervisor, as: WorkerSupervisor
alias WC.Miner.Worker, as: MinerWorker

require Logger

defmodule WC.Miner.MinerServer do
  use GenServer

  @initial_state %{pids: [], mining_processes: nil}
  
  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def new_block(tip) do
    GenServer.cast(__MODULE__, {:new_block, tip})
  end
  
  def init([]) do
    mining_processes = Application.get_env(:wc, :mining_processes, 1)
    if LogServer.index_complete? do
      {:ok, tip} = LogServer.get_tip()
      :ok = new_block(tip)
    end
    {:ok, %{@initial_state | mining_processes: mining_processes}}
  end

  def handle_cast({:new_block, tip}, state = %{pids: pids, mining_processes: mining_processes}) do
    for pid <- pids, do: MinerWorker.stop(pid)
    new_block = Block.next_block(tip)
    # Logger.info "Mining new block #{BlockHeader.pprint(new_block.header)}"
    # TODO: get tx's for new block from mempool
    new_pids = Enum.map(1 .. mining_processes, fn _ ->
      {:ok, pid} = WorkerSupervisor.start_child(new_block)
      pid
    end)
    {:noreply, %{state | pids: new_pids}}
  end

end

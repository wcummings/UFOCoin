alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.InvItem, as: InvItem
alias WC.Blockchain.Block, as: Block
alias WC.P2P.Connection, as: P2PConnection
alias WC.P2P.Packet, as: P2PPacket
require Logger

alias WC.P2P.Connection, as: P2PConnection
alias WC.P2P.Packet, as: P2PPacket
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.InvItem, as: InvItem

defmodule WC.Blockchain.InventoryServer do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Process.send_after(self(), :getblocks_timer, 60 * 1000)    
    {:ok, %{}}
  end

  def getblocks do
    GenServer.cast(__MODULE__, :getblocks)
  end

  def handle_cast(:getblocks, state) do
    send_getblocks()
    {:noreply, state}
  end

  def handle_info(:getblocks_timer, state) do
    send_getblocks()
    Process.send_after(self(), :getblocks_timer, 60 * 1000)
    {:noreply, state}
  end

  def send_getblocks do
    # Check that we don't spam the network during the initial indexing    
    if LogServer.index_complete? do
      :ok = P2PConnection.broadcast(%P2PPacket{proc: :getblocks, extra_data: get_block_locator()})
    end
  end
  
  @doc "Build a list of block hashes from newest to genesis, dense to start, then sparse"
  @spec get_block_locator() :: list(Block.block_hash)
  def get_block_locator do
    {:ok, tip} = LogServer.get_tip()
    get_block_locator(tip)
  end
  
  def get_block_locator(tip) do
    genesis_block_hash = Block.hash(WC.genesis_block)
    dense_hashes = for block <- LogServer.get_prev_blocks(10, tip), do: Block.hash(block)
    if tip.header.height < 10 do
      dense_hashes ++ [genesis_block_hash]
    else
      dense_hashes ++ get_prev_block_hashes_sparse(tip) ++ [genesis_block_hash]
    end
  end

  def get_prev_block_hashes_sparse(tip) do
    get_prev_block_hashes_sparse(tip, 1, 0, [])
  end
  
  def get_prev_block_hashes_sparse(tip, step, count, acc) do
    {:ok, block} = LogServer.get_block_by_hash(tip.header.prev_block_hash)
    if block.header.height == 0 do
      acc
    else
      if count == step do
	get_prev_block_hashes_sparse(block, step * 2, 0, [Block.hash(block)|acc])
      else
	get_prev_block_hashes_sparse(block, step, count + 1, acc)
      end
    end
  end
  
end

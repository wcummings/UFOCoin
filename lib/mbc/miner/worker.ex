require Logger

alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.LogServer, as: LogServer
alias MBC.P2P.Connection, as: P2PConnection
alias MBC.P2P.Packet, as: P2PPacket

defmodule MBC.Miner.Worker do

  def child_spec(_opts) do
    %{
      id: __MODULE__,
      restart: :transient,
      shutdown: 5000,
      start: {__MODULE__, :start_link, []},
      type: :worker
    }
  end
  
  def start_link(block) do
    {:ok, spawn_link fn -> mine(block) end}
  end

  def stop(pid) do
    send pid, :stop
  end

  def mine(block) do
    mine(block, MBC.Util.difficulty_to_target(block.difficulty))
  end

  def mine(block = %Block{}, target) do
    mine(Block.encode(%{block | nonce: :crypto.strong_rand_bytes(4)}), target)
  end

  def mine(block, target) when is_binary(block) do
    case Block.check_nonce(block) do
      {true, hash} ->
	Logger.info "Successfully mined block, difficulty = #{target}, block_hash = #{Base.encode16(hash)}"
	:ok = LogServer.update(block)
	P2PConnection.broadcast(%P2PPacket{proc: :block, extra_data: Block.decode(block)})	
      {false, _} ->
	receive do
	  :stop ->
	    exit(:normal)
	after 1500 ->
	    mine(Block.update_nonce(block, :crypto.strong_rand_bytes(4)), target)
	end
    end
  end

end

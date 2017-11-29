require Logger

alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockHeader, as: BlockHeader
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
    mine(block, MBC.Util.difficulty_to_target(block.header.difficulty))
  end

  def mine(block = %Block{header: block_header}, target) do
    valid_block_header = mine(BlockHeader.encode(%{block_header | nonce: :crypto.strong_rand_bytes(4)}), target)
    hash = BlockHeader.hash(valid_block_header)
    Logger.info "Successfully mined block, difficulty = #{target}, block_hash = #{Base.encode16(hash)}"
    new_block = %{block | header: BlockHeader.decode(valid_block_header)}
    :ok = LogServer.update(new_block)
    P2PConnection.broadcast(%P2PPacket{proc: :block, extra_data: Block.decode(block)})    
  end

  def mine(block_header, target) when is_binary(block_header) do
    case BlockHeader.check_nonce(block_header) do
      {true, _hash} ->
	block_header
      {false, _} ->
	receive do
	  :stop ->
	    exit(:normal)
	after 1000 ->
	    mine(BlockHeader.update_nonce(block_header, :crypto.strong_rand_bytes(4)), target)
	end
    end
  end

end

require Logger

alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.OrphanBlockTable, as: OrphanBlockTable
alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.UTXOSet, as: UTXOSet
alias WC.P2P.Packet, as: P2PPacket
alias WC.P2P.ConnectionRegistry, as: P2PConnectionRegistry

defmodule WC.Blockchain.BlockValidatorServer do
  @moduledoc """
  Serialize block validation, so we never validate the same block twice
  """
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @spec validate_block(Block.t) :: :ok | {:error, Block.block_validation_error}
  def validate_block(block) do
    GenServer.cast(__MODULE__, {:validate_block, block})
  end

  def init([]) do
    {:ok, %{}}
  end

  def handle_cast({:validate_block, block = %Block{}}, state) do
    block_hash = Block.hash(block)
    result = LogServer.get_block_by_hash(block_hash)
    case result do
      {:ok, _block} ->
	{:noreply, state}	
      {:error, :notfound} ->
	case validate(block) do
	  :ok ->
	    Logger.info "Block accepted: #{BlockHeader.pprint(block.header)}"	    
	    :ok = LogServer.update(block)
	    case OrphanBlockTable.get_by_prev_block_hash(block_hash) do
	      {:ok, blocks} ->
		Logger.info "Found orphan child blocks: #{inspect(Enum.map(blocks, &Block.hash/1) |> Enum.map(&Base.encode16/1))}"
		Enum.each(blocks, &validate_block/1)
		{:noreply, state}		
	      {:error, :notfound} ->
		{:noreply, state}
	    end
 	  {:error, :orphan} ->
	    # See Block.validate/1 in block.ex for details.
	    :ok = OrphanBlockTable.insert(block)
	    :ok = send_getblocks()
	    {:noreply, state}
	  {:error, error} ->
	    Logger.warn "Block rejected, reason: #{inspect(error)}, #{BlockHeader.pprint(block.header)}"	    
	    {:noreply, state}
	end
    end
  end

  #
  # PRIVATE
  #

  @spec validate(Block.t) :: {:ok, Block.t} | {:error, Block.block_validation_error}
  def validate(block) do
    # Check nonce first, don't want to do any work on a block w/ invalid PoW to prevent DoS
    case BlockHeader.check_nonce(block.header) do
      {true, _} ->
	check_prev_block(block)
      {false, _} ->
	{:error, :badnonce}
    end
  end

  def check_fake_merkle_hash(%Block{header: %BlockHeader{fake_merkle_hash: fake_merkle_hash}, txs: txs} = block) do
    if :crypto.hash(:sha256, Enum.map(txs, &TX.encode/1)) == fake_merkle_hash do
      check_prev_block(block)
    else
      {:error, :badmerklehash}
    end
  end

  def validate_coinbase(block = %Block{txs: [coinbase|_]}) do
    if TX.is_coinbase?(coinbase) do
      validate_txs(block)
    else
      {:error, :invalid_tx}
    end
  end

  def validate_txs(block = %Block{txs: [_|txs]}) do
    tx_error = Enum.find(txs, fn tx -> validate_tx(tx) != :ok end)
    if tx_error != nil do
      tx_error
    else
      check_prev_block(block)
    end
  end

  @spec validate_tx(TX.t) :: true | false
  def validate_tx(tx) do
    case get_outputs(tx) do
      {:error, error} ->
	{:error, error}
      {:ok, outputs} ->
	total_input_value = get_total_output_value(outputs)
	total_output_value = get_total_output_value(tx.outputs)
	fee = total_input_value - total_output_value
	if fee < 0 do
	  {:error, :not_enough_value}
	else
	  first_invalid_input = Enum.with_index(outputs)
	  |> Enum.map(fn {i, output} -> {output, tx.inputs[i]} end)
	  |> Enum.find(fn {input, output} -> not Input.validate(input, output, TX.encode(tx)) end)
	  if first_invalid_input != nil do
	    {:error, {:bad_input, first_invalid_input}}
	  else
	    :ok
	  end
	end
    end
  end

  def check_prev_block(block = %Block{header: %BlockHeader{prev_block_hash: prev_block_hash}}) do
    case LogServer.get_block_by_hash(prev_block_hash) do
      {:error, :notfound} ->
	{:error, :orphan}
      {:ok, prev_block} ->
	check_prev_block(block, prev_block)
    end
  end

  def check_prev_block(block, prev_block) do
    if (block.header.height - 1) == prev_block.header.height do
      check_difficulty(block)
    else
      {:error, :badheight}
    end
  end

  def check_difficulty(block) do
    difficulty = Block.get_difficulty(block)
    if block.header.difficulty >= difficulty do
      :ok
    else
      {:error, :baddifficulty}
    end
  end

  def get_total_output_value(outputs) do
    Enum.map(outputs, fn output -> output.value end) |> Enum.sum
  end

  def get_outputs(inputs), do: get_outputs(inputs, [])
  
  def get_outputs([%Input{tx_hash: tx_hash, offset: offset}|rest], acc) do
    case UTXOSet.get_output(tx_hash, offset) do
      {:error, :notfound} ->
	{:error, {:missing_output, {tx_hash, offset}}}
      {:ok, output} ->
	get_outputs(rest, [output|acc])
    end
  end

  def get_outputs([], acc), do: {:ok, Enum.reverse(acc)}
  
  # FIXME: get rid of this!
  def send_getblocks do
    # Check that we don't spam the network during the initial indexing    
    # if LogServer.index_complete?() do
    :ok = P2PConnectionRegistry.broadcast("packet", %P2PPacket{proc: :getblocks, extra_data: LogServer.make_block_locator()})
    # end
  end
  
end

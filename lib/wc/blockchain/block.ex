require Logger

alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.LogServer, as: LogServer
alias WC.Blockchain.UTXODb, as: UTXODb

defmodule WC.Blockchain.Block do
  @enforce_keys [:header, :txs]
  defstruct [:header, :txs]

  @type block_validation_error :: :notfound | :orphan | :bad_height | :already_accepted | :bad_difficulty | :bad_merkle_hash
  @type encoded_block :: iodata
  @type t :: %__MODULE__{header: BlockHeader.t, txs: list(TX.t)}

  @spec genesis_block() :: t
  def genesis_block do
    %__MODULE__{header: %BlockHeader{prev_block_hash: <<0 :: size(256)>>,
				     difficulty: 1,
				     height: 0,
				     fake_merkle_hash: <<0 :: size(256)>>,
				     timestamp: 0}, txs: []}    
  end
  
  @spec encode(t) :: encoded_block
  def encode(%__MODULE__{header: block_header, txs: txs}) do
    # Create a compact representation of the transaction
    # Should eventually be replaced by a merkle hash, so
    # that you can check membership
    encoded_txs = Enum.map(txs, &TX.encode/1)
    fake_merkle_hash = :crypto.hash(:sha256, encoded_txs)    
    [
      BlockHeader.encode(%{block_header | fake_merkle_hash: fake_merkle_hash}),
      <<:erlang.iolist_size(encoded_txs) :: size(32)>>,
      encoded_txs
    ]
  end

  # Block header size must be updated if header format is changed
  # Might be able to handle this w/ a macro
  @spec decode(encoded_block) :: t
  def decode(<<encoded_block_header :: binary-size(86), length :: size(32), txs :: binary-size(length)>>) do
    decoded_txs = case TX.decode(txs) do
      [coinbase|decoded_txs] -> [%{coinbase | is_coinbase: true}|decoded_txs]
      [] -> []
    end
    %__MODULE__{header: BlockHeader.decode(encoded_block_header), txs: decoded_txs}
  end

  @spec hash(t) :: BlockHeader.block_hash
  def hash(%__MODULE__{header: block_header, txs: txs}) do
    encoded_txs = Enum.map(txs, &TX.encode/1)
    BlockHeader.hash(%{block_header | fake_merkle_hash: :crypto.hash(:sha256, encoded_txs)})
  end

  @spec hash(encoded_block) :: BlockHeader.block_hash
  def hash(<<encoded_block_header :: binary-size(86), _ :: binary>>) do
    BlockHeader.hash(encoded_block_header)
  end

  @spec next_block(t) :: t
  def next_block(prev_block = %__MODULE__{header: prev_block_header}) do
    prev_block_hash = BlockHeader.hash(prev_block_header)
    txs = [TX.coinbase()]
    block_header = %BlockHeader{prev_block_hash: prev_block_hash,
				height: prev_block_header.height + 1,
				difficulty: get_current_difficulty(prev_block),
				fake_merkle_hash: :crypto.hash(:sha256, Enum.map(txs, &TX.encode/1)),
				timestamp: :os.system_time(:millisecond)}
    %__MODULE__{header: block_header, txs: txs}
  end

  @spec equal?(t, t) :: true | false
  def equal?(block1, block2) do
    Map.equal?(block1, block2)
  end

  # FIXME: wrap in max(), so difficulty can only change so much per-day
  @spec get_difficulty(list(Block.t)) :: non_neg_integer
  def get_difficulty(blocks) when is_list(blocks) do
    case length(blocks) do
      0 ->
	1
      i when i < 2 ->
	block = List.last(blocks)
	block.header.difficulty
      _ ->
	first = List.first(blocks)
	last = List.last(blocks)
	if (last.header.timestamp - first.header.timestamp) < (24 * 60 * 60 * 1000) do
	  last.header.difficulty + 1
	else
	  WC.Util.max(last.header.difficulty - 1, 1)
	end
    end
  end

  @spec validate_without_utxodb(Block.t, non_neg_integer(), Block.t) :: {:ok , block_validation_error}
  def validate_without_utxodb(prev_block, difficulty, block) do
    validators = [
      &validate_nonce/1,
      &validate_fake_merkle_hash/1,
      &validate_coinbase/1,
      fn block -> validate_prev_block(prev_block, block) end,
      fn block -> validate_difficulty(difficulty, block) end,
    ]
    case Enum.find(validators, fn validator -> validator.(block) != :ok end) do
      nil ->
	:ok
      error ->
	error
    end    
  end
  
  @spec validate(UTXODb.t, Block.t, non_neg_integer, Block.t) :: {:ok, Block.t} | {:error, block_validation_error}
  def validate(db, prev_block, difficulty, block) do
    validators = [
      fn block -> validate_without_utxodb(prev_block, difficulty, block) end,
      fn block -> validate_txs(db, block) end
    ]
    case Enum.find(validators, fn validator -> validator.(block) != :ok end) do
      nil ->
	:ok
      error ->
	error
    end
  end

  @spec difficulty_number_of_blocks :: non_neg_integer
  def difficulty_number_of_blocks do
    144
  end
  
  #
  # VALIDATORS
  #
  
  def validate_nonce(block) do
    case BlockHeader.check_nonce(block.header) do
      {true, _} ->
	:ok
      {false, _} ->
	{:error, :bad_nonce}
    end
  end

  def validate_fake_merkle_hash(%__MODULE__{header: %BlockHeader{fake_merkle_hash: fake_merkle_hash}, txs: txs}) do
    if :crypto.hash(:sha256, Enum.map(txs, &TX.encode/1)) == fake_merkle_hash do
      :ok
    else
      {:error, :bad_merkle_hash}
    end
  end

  def validate_prev_block(prev_block, block) do
    if (block.header.height - 1) == prev_block.header.height do
      :ok
    else
      {:error, :bad_height}
    end
  end

  def validate_difficulty(difficulty, block) do
    if block.header.difficulty >= difficulty do
      :ok
    else
      {:error, :bad_difficulty}
    end
  end

  def validate_txs(db, %__MODULE__{txs: [_|txs]}) do
    case Enum.find(txs, fn tx -> validate_tx(db, tx) != :ok end) do
      nil ->
	:ok
      error ->
	error
    end
  end
 
  @spec validate_tx(UTXODb.t, TX.t) :: :ok | {:error, term()}
  def validate_tx(db, tx) do
    case get_outputs(db, tx) do
      {:ok, outputs} ->
	TX.verify(tx, outputs)
      error ->
	error
    end
  end

  def validate_coinbase(%__MODULE__{txs: [coinbase|_]}) do
    if TX.is_coinbase?(coinbase) do
      :ok
    else
      {:error, :invalid_coinbase}
    end
  end

  #
  # PRIVATE
  #
  
  def get_outputs(db, inputs), do: get_outputs(db, inputs, %{})
  
  def get_outputs(db, [%Input{tx_hash: tx_hash, offset: offset}|rest], acc) do
    case UTXODb.get(db, tx_hash, offset) do
      {:error, :notfound} ->
	{:error, {:missing_output, {tx_hash, offset}}}
      {:ok, output} ->
	get_outputs(db, rest, Map.put(acc, {tx_hash, offset}, output))
    end
  end

  def get_outputs(_, [], acc), do: {:ok, acc}

  #
  # PRIVATE
  #
  
  def get_current_difficulty(block) do
    blocks = LogServer.find_prev_blocks(144, block) ++ [block]
    get_difficulty(blocks)
  end

end

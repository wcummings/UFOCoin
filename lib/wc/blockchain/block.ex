require Logger

alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.LogServer, as: LogServer

defmodule WC.Blockchain.Block do
  @enforce_keys [:header, :txs]
  defstruct [:header, :txs]

  @type block_validation_error :: :notfound | :orphan | :badheight | :alreadyaccepted | :baddifficulty | :badmerklehash
  @type encoded_block :: binary
  @type t :: %__MODULE__{header: BlockHeader.t, txs: list(TX.t)}

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
    %__MODULE__{header: BlockHeader.decode(encoded_block_header), txs: TX.decode(txs)}
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

  # FIXME: pass previous block, and difficulty etc. as parameters here.
  # Prepare them in BlockValidatorServer
  @spec validate(t) :: :ok | {:error, block_validation_error}
  def validate(new_block = %__MODULE__{header: block_header}) do
    case BlockHeader.check_nonce(block_header) do
      {true, _} ->
	check_prev_block(new_block)
      {false, _} ->
	{:error, :badnonce}
    end
  end

  def check_fake_merkle_hash(%__MODULE__{header: %BlockHeader{fake_merkle_hash: fake_merkle_hash}, txs: txs} = block) do
    if :crypto.hash(:sha256, Enum.map(txs, &TX.encode/1)) == fake_merkle_hash do
      check_prev_block(block)
    else
      {:error, :badmerklehash}
    end
  end

  def check_prev_block(block = %__MODULE__{header: %BlockHeader{prev_block_hash: prev_block_hash}}) do
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
    difficulty = get_difficulty(block)
    if block.header.difficulty >= difficulty do
      :ok
    else
      {:error, :baddifficulty}
    end
  end

  # Includes block passed as argument in calculation, for generating new blocks.
  def get_current_difficulty(block) do
    blocks = LogServer.find_prev_blocks(144, block) ++ [block]
    get_difficulty(blocks)
  end

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
	  last.header.difficulty - 1
	end
    end
  end

  def get_difficulty(block) do
    get_difficulty(LogServer.find_prev_blocks(144, block))
  end

end

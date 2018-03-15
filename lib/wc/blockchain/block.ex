require Logger

alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.LogServer, as: LogServer

defmodule WC.Blockchain.Block do
  @enforce_keys [:header, :txs]
  defstruct [:header, :txs]

  @type block_validation_error :: :notfound | :orphan | :badheight | :alreadyaccepted | :baddifficulty
  @type encoded_block :: binary
  @type t :: %__MODULE__{header: BlockHeader.t, txs: list(TX.t)}

  @spec encode(t) :: encoded_block
  def encode(%__MODULE__{header: block_header, txs: _txs}) do
    BlockHeader.encode(block_header) # FIXME: Ignoring TX's for now
  end

  # Block header size must be updated if header format is changed
  # Might be able to handle this w/ a macro
  @spec decode(encoded_block) :: t
  def decode(encoded_block_header) do
    block_header = BlockHeader.decode(encoded_block_header)
    %__MODULE__{header: block_header, txs: []} # FIXME: Ignoring TX's for now
  end

  @spec hash(t) :: BlockHeader.block_hash
  def hash(%__MODULE__{header: block_header}) do
    BlockHeader.hash(block_header)
  end

  @spec hash(encoded_block) :: BlockHeader.block_hash
  def hash(block_header) do
    BlockHeader.hash(block_header)
  end

  @spec next_block(t) :: t
  def next_block(prev_block = %__MODULE__{header: prev_block_header, txs: _txs}) do
    prev_block_hash = BlockHeader.hash(prev_block_header)
    block_header = %BlockHeader{prev_block_hash: prev_block_hash,
				height: prev_block_header.height + 1,
				difficulty: get_current_difficulty(prev_block),
				timestamp: :os.system_time(:millisecond)}
    %__MODULE__{header: block_header, txs: []}
  end

  @spec equal?(t, t) :: true | false
  def equal?(block1, block2) do
    Map.equal?(block1, block2)
  end

  # IMPORTANT NOTE: This should check if the block is orphaned LAST,
  # the caller should be able to trust that an orphaned block is valid,
  # aside from being an orphan.
  @spec validate(t) :: :ok | {:error, block_validation_error}
  def validate(new_block = %__MODULE__{header: block_header}) do
    case BlockHeader.check_nonce(block_header) do
      {true, _} ->
	check_prev_block(new_block)
      {false, _} ->
	{:error, :badnonce}
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
    blocks = LogServer.get_prev_blocks(144, block) ++ [block]
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
    get_difficulty(LogServer.get_prev_blocks(144, block))
  end
  
end

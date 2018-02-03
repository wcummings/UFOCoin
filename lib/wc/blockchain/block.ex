require Logger

alias WC.Blockchain.BlockHeader, as: BlockHeader
alias WC.Blockchain.TX, as: TX
alias WC.Blockchain.LogServer, as: LogServer

defmodule WC.Blockchain.Block do
  @enforce_keys [:header, :txs]
  defstruct [:header, :txs]

  @hardcoded_difficulty 4
  
  @type block_validation_error :: :notfound | :orphan | :badheight | :alreadyaccepted
  @type encoded_block :: binary
  @type t :: %WC.Blockchain.Block{header: BlockHeader.t, txs: list(TX.t)}

  @spec encode(t) :: encoded_block
  def encode(%WC.Blockchain.Block{header: block_header, txs: _txs}) do
    BlockHeader.encode(block_header) # FIXME: Ignoring TX's for now
  end

  # Block header size must be updated if header format is changed
  # Might be able to handle this w/ a macro
  @spec decode(encoded_block) :: t
  def decode(encoded_block_header) do
    block_header = BlockHeader.decode(encoded_block_header)
    %WC.Blockchain.Block{header: block_header, txs: []} # FIXME: Ignoring TX's for now
  end

  @spec hash(t) :: BlockHeader.block_hash
  def hash(%WC.Blockchain.Block{header: block_header}) do
    BlockHeader.hash(block_header)
  end

  @spec hash(encoded_block) :: BlockHeader.block_hash
  def hash(block_header) do
    BlockHeader.hash(block_header)
  end

  @spec next_block(t) :: t
  def next_block(%WC.Blockchain.Block{header: prev_block_header, txs: _txs}) do
    prev_block_hash = BlockHeader.hash(prev_block_header)
    block_header = %BlockHeader{prev_block_hash: prev_block_hash,
				height: prev_block_header.height + 1,
				difficulty: @hardcoded_difficulty,
				timestamp: :os.system_time(:millisecond)}
    %WC.Blockchain.Block{header: block_header, txs: []}
  end

  @spec equal?(Block.t, Block.t) :: true | false
  def equal?(block1, block2) do
    Map.equal?(block1, block2)
  end

  # IMPORTANT NOTE: This should check if the block is orphaned LAST,
  # the caller should be able to trust that an orphaned block is valid,
  # aside from being an orphan.
  @spec validate(t) :: :ok | {:error, block_validation_error}
  def validate(new_block = %WC.Blockchain.Block{header: block_header}) do
    case BlockHeader.check_nonce(block_header) do
      {true, _} ->
	check_prev_block(new_block)
      {false, _} ->
	{:error, :badnonce}
    end
  end

  def check_prev_block(block = %WC.Blockchain.Block{header: %BlockHeader{prev_block_hash: prev_block_hash}}) do
    case LogServer.get_block_by_hash(prev_block_hash) do
      {:error, :notfound} ->
	{:error, :orphan}
      {:ok, prev_block} ->
	check_prev_block(block, prev_block)
    end
  end

  def check_prev_block(block, prev_block) do
    if (block.header.height - 1) == prev_block.header.height do
      :ok
    else
      {:error, :badheight}
    end
  end
  
end

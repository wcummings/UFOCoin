require Logger

alias MBC.Blockchain.BlockHeader, as: BlockHeader
alias MBC.Blockchain.TX, as: TX
alias MBC.Blockchain.LogServer, as: LogServer

defmodule MBC.Blockchain.Block do
  @enforce_keys [:header, :txs]
  defstruct [:header, :txs]

  @type block_hash :: binary()
  @type t :: %MBC.Blockchain.Block{header: BlockHeader.t, txs: list(TX.t)}

  def encode(%MBC.Blockchain.Block{header: block_header, txs: _txs}) do
    BlockHeader.encode(block_header) # FIXME: Ignoring TX's for now
  end

  # Block header size must be updated if header format is changed
  # Might be able to handle this w/ a macro  
  def decode(header) do
    block_header = BlockHeader.decode(header)
    %MBC.Blockchain.Block{header: block_header, txs: []} # FIXME: Ignoring TX's for now
  end

  def hash(%MBC.Blockchain.Block{header: header}) do
    BlockHeader.hash(header)
  end

  def hash(block_header) do
    BlockHeader.hash(block_header)
  end

  def next_block(%MBC.Blockchain.Block{header: prev_block_header, txs: _txs}) do
    prev_block_hash = BlockHeader.hash(prev_block_header)
    block_header = %BlockHeader{prev_block_hash: prev_block_hash,
				height: prev_block_header.height + 1,
				difficulty: 8,
				timestamp: :os.system_time(:millisecond)}
    %MBC.Blockchain.Block{header: block_header, txs: []}
  end

  def validate(new_block = %MBC.Blockchain.Block{header: block_header}) do
    if BlockHeader.check_nonce(block_header) do
      check_prev_block(new_block)
    else
      {:error, :badnonce}
    end
  end

  def check_prev_block(block = %MBC.Blockchain.Block{header: %BlockHeader{prev_block_hash: prev_block_hash}}) do
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

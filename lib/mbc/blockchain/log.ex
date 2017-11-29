require Logger

alias MBC.Blockchain.Block, as: Block
alias MBC.Blockchain.BlockHashIndex, as: BlockHashIndex

defmodule MBC.Blockchain.Log do
  @enforce_keys [:file, :path]
  defstruct [:file, :path]

  @opaque t :: %MBC.Blockchain.Log{}

  @spec init() :: {:ok, MBC.Blockchain.Log.t} | {:error, :_}
  def init do
    data_dir = Application.get_env(:mbc, :data_dir)
    init(Path.join([data_dir, "blocks.dat"]))
  end
  
  @spec init(String.t) :: {:ok, MBC.Blockchain.Log.t} | {:error, :_}
  def init(path) do
    case :file.open(path, [:read, :append, :binary, :raw]) do
      {:ok, file} ->
	{:ok, %MBC.Blockchain.Log{file: file, path: path}}
      {:error, error} ->
	{:error, error}
    end
  end

  @spec is_empty?(MBC.Blockchain.Log.t) :: true | false
  def is_empty?(%MBC.Blockchain.Log{file: file}) do
    {:ok, offset} = :file.position(file, :eof)
    offset == 0
  end
  
  @spec read_block(MBC.Blockchain.Log.t, non_neg_integer()) :: {:ok, {binary(), non_neg_integer()}} | {:error, :eof}
  def read_block(%MBC.Blockchain.Log{file: file}, offset) do
    {:ok, ^offset} = :file.position(file, offset)
    # Read 4 byte length prefix
    case :file.pread(file, offset, 4) do
      {:ok, bin_length} ->
	length = decode_length(bin_length)
	{:ok, encoded_block} = :file.pread(file, offset + 4, length)
	# Return offset to the next block w/ the retrieved block for indexing
	{:ok, {encoded_block, offset + 4 + length}}	    
      :eof ->
	{:error, :eof}
    end
  end

  @spec append_block(MBC.Blockchain.Log.t, binary()) :: non_neg_integer()
  def append_block(%MBC.Blockchain.Log{file: file}, encoded_block) do
    {:ok, offset} = :file.position(file, :eof)
    :ok = :file.write(file, encode_length(byte_size(encoded_block)) <> encoded_block)
    offset
  end

  @spec encode_length(non_neg_integer()) :: binary()
  def encode_length(length) do
    <<length :: size(32)>>
  end

  @spec decode_length(binary()) :: non_neg_integer()
  def decode_length(length) do
    :binary.decode_unsigned(length)
  end

  @spec index_blocks(MBC.Blockchain.Log.t) :: Block.t
  def index_blocks(log) do
    index_blocks(log, 0, MBC.genesis_block)
  end
  
  def index_blocks(log, offset, tip) do
    case read_block(log, offset) do
      {:ok, {encoded_block, next_offset}} ->
	block_hash = Block.hash(encoded_block)
	Logger.info "Indexing... #{Base.encode16(block_hash)}"
	:ok = BlockHashIndex.insert(block_hash, offset)
	block = Block.decode(encoded_block)
	new_tip = if block.header.height > tip.header.height do
	  block
	else
	  tip
	end
	index_blocks(log, next_offset, new_tip)
      {:error, :eof} ->
	tip
    end
  end
  
end

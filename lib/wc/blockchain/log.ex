require Logger

alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHashIndex, as: BlockHashIndex

defmodule WC.Blockchain.Log do
  @enforce_keys [:file, :path]
  defstruct [:file, :path]

  @opaque t :: %WC.Blockchain.Log{}

  @spec init() :: {:ok, WC.Blockchain.Log.t} | {:error, :_}
  def init do
    data_dir = Application.get_env(:wc, :data_dir)
    init(Path.join([data_dir, "blocks.dat"]))
  end
  
  @spec init(String.t) :: {:ok, WC.Blockchain.Log.t} | {:error, :_}
  def init(path) do
    case :file.open(path, [:read, :append, :binary, :raw]) do
      {:ok, file} ->
	{:ok, %WC.Blockchain.Log{file: file, path: path}}
      {:error, error} ->
	{:error, error}
    end
  end

  @spec is_empty?(WC.Blockchain.Log.t) :: true | false
  def is_empty?(%WC.Blockchain.Log{file: file}) do
    {:ok, offset} = :file.position(file, :eof)
    offset == 0
  end
  
  @spec read_block(WC.Blockchain.Log.t, non_neg_integer()) :: {:ok, {binary(), non_neg_integer()}} | {:error, :eof}
  def read_block(%WC.Blockchain.Log{file: file}, offset) do
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

  @spec append_block(WC.Blockchain.Log.t, binary()) :: non_neg_integer()
  def append_block(%WC.Blockchain.Log{file: file}, encoded_block) do
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

  @spec index_blocks(WC.Blockchain.Log.t) :: Block.t
  def index_blocks(log) do
    index_blocks(log, 0, WC.genesis_block)
  end
  
end

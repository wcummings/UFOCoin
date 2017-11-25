defmodule MBC.Blockchain.Log do
  @enforce_keys [:file, :path]
  defstruct [:file, :path]

  @opaque t :: %MBC.Blockchain.Log{}

  @spec init(String.t) :: {:ok, MBC.Blockchain.Log.t} | {:error, :_}
  def init(path) do
    case :file.open(path, [:read, :append, :binary, :raw]) do
      {:ok, file} ->
	{:ok, %MBC.Blockchain.Log{file: file, path: path}}
      {:error, error} ->
	{:error, error}
    end
  end

  @spec read_block(MBC.Blockchain.Log.t, non_neg_integer()) :: {binary(), non_neg_integer()}
  def read_block(%MBC.Blockchain.Log{file: file}, offset) do
    {:ok, ^offset} = :file.position(file, offset)
    # Read 4 byte length prefix
    {:ok, bin_length} = :file.pread(file, offset, 4)
    length = decode_length(bin_length)
    {:ok, encoded_block} = :file.pread(file, offset + 4, length)
    # Return offset to the next block w/ the retrieved block for indexing
    {encoded_block, offset + 4 + length}
  end

  @spec append_block(MBC.Blockchain.Log.t, binary()) :: :ok
  def append_block(%MBC.Blockchain.Log{file: file}, encoded_block) do
    {:ok, offset} = :file.position(file, :eof)
    :ok = :file.write(file, encode_length(byte_size(encoded_block)) <> encoded_block)
    {:ok, offset}
  end

  @spec encode_length(non_neg_integer()) :: binary()
  def encode_length(length) do
    <<length :: size(32)>>
  end

  @spec decode_length(binary()) :: non_neg_integer()
  def decode_length(length) do
    :binary.decode_unsigned(length)
  end
  
end

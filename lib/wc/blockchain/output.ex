defmodule WC.Blockchain.Output do
  defstruct [:fingerprint, :value]

  @type t :: %__MODULE__{}
  @type encoded_output :: binary

  @spec encode(t) :: encoded_output
  def encode(%__MODULE__{fingerprint: <<fingerprint :: binary-size(32)>>, value: value}) do
    <<fingerprint :: binary, value :: size(32)>>
  end

  @spec decode(encoded_output) :: t
  def decode(<<fingerprint :: binary-size(32), value :: size(32)>>) do
    %__MODULE__{fingerprint: fingerprint, value: value}
  end
  
end

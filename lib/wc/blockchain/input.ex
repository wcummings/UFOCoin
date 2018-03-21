defmodule WC.Blockchain.Input do
  @enforce_keys [:tx_hash, :offset, :pubkey]
  defstruct [:tx_hash, :offset, :pubkey, :signature]

  @type t :: %__MODULE__{}
  @type encoded_input :: binary

  # @spec sign(t, binary) :: t
  # def sign(%__MODULE__{tx_hash: <<tx_hash :: binary - size(32)>>,
  # 		       offset: offset,
  # 		       pubkey: <<pubkey :: binary - size(32)>>} = input, privkey) do
  #   # input_without_sig = <<tx_hash :: binary, offset :: size(8), pubkey :: binary>>
  #   # %{input | signature: :crypto.sign(:rsa, :rsa_digest_type, input_without_sig, privkey)}
  #   # FIXME: signed w/ contents of full transaction
  # end

  @spec encode(t) :: encoded_input
  def encode(%__MODULE__{tx_hash: <<tx_hash :: binary - size(32)>>,
			 offset: offset,
			 pubkey: <<pubkey :: binary - size(32)>>,
			 signature: <<signature :: binary - size(32)>>}) when signature != nil do
    <<tx_hash :: binary, offset :: size(8), pubkey :: binary, signature :: binary>>
  end

  @spec decode(encoded_input) :: t
  def decode(<<tx_hash :: binary - size(32),
               offset :: size(8),
               pubkey :: binary - size(32),
               signature :: binary - size(256)>>) do
    %__MODULE__{tx_hash: tx_hash, offset: offset, pubkey: pubkey, signature: signature}
  end

end

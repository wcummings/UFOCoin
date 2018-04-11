alias WC.Blockchain.Output, as: Output
alias WC.Blockchain.TX, as: TX
alias WC.Wallet.KeyStore, as: KeyStore

defmodule WC.Blockchain.Input do
  @enforce_keys [:tx_hash, :offset, :pubkey]
  defstruct [:tx_hash, :offset, :pubkey, :signature]

  @type t :: %__MODULE__{}
  @type encoded_input :: binary
  @type offset :: non_neg_integer

  @spec encode(t) :: encoded_input
  def encode(%__MODULE__{tx_hash: <<tx_hash :: binary-size(32)>>,
			 offset: offset,
			 pubkey: <<pubkey :: binary-size(32)>>,
			 signature: <<signature :: binary-size(256)>>}) do
    <<tx_hash :: binary, offset :: size(8), pubkey :: binary, signature :: binary>>
  end

  @spec decode(encoded_input) :: t
  def decode(<<tx_hash :: binary-size(32),
               offset :: size(8),
               pubkey :: binary-size(32),
               signature :: binary-size(256)>>) do
    %__MODULE__{tx_hash: tx_hash, offset: offset, pubkey: pubkey, signature: signature}
  end

  @spec sign(t, TX.encoded_tx, binary) :: t
  def sign(input, encoded_tx, private_key) do
    %{input | signature: :crypto.sign(:rsa, :rsa_digest_type, encoded_tx, private_key)}
  end
  
  @spec validate(t, Output.t, TX.encoded_tx) :: true | false
  def validate(%__MODULE__{pubkey: pubkey, signature: signature}, %Output{fingerprint: fingerprint}, encoded_tx) do
    fingerprint == KeyStore.fingerprint(pubkey) and
    :crypto.verify(:rsa,
      :rsa_digest_type,
      encoded_tx,
      signature,
      pubkey)
  end

end

alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output

defmodule WC.Blockchain.TX do
  defstruct version: 1, inputs: [], outputs: []

  @type t :: %__MODULE__{version: non_neg_integer, inputs: list(Input.t), outputs: list(Output.t)}
  @type encoded_tx :: binary

  #
  # PUBLIC
  #
  
  @spec sign(binary, t) :: t
  def sign(privkey, %__MODULE__{version: 1, inputs: inputs, outputs: outputs} = tx) do
    # Set all input signatures to 0 to create the message we're going to sign
    inputs2 = Enum.map(inputs, fn input -> %{input | signature: <<0 :: size(2048)>>} end)
    # Encode full tx, w/ zero'd signatures
    encoded_tx_without_sig = encode(%{tx | inputs: inputs2})
    # Use encoded tx as message for signature
    signature = :crypto.sign(:rsa, :rsa_digest_type, encoded_tx_without_sig, privkey)
    inputs3 = Enum.map(inputs2, fn input -> %{input | signature: signature} end)
    %{tx | inputs: inputs3}
  end
  
  @spec encode(t) :: encoded_tx
  def encode(%__MODULE__{inputs: inputs, outputs: outputs}) do
    [
      <<0x01>>,
      <<byte_size(inputs) :: size(8)>>,
      Enum.map(inputs, &Input.encode/1),
      <<byte_size(outputs) :: size(8)>>,      
      Enum.map(outputs, &Output.encode/1)
    ]
  end

  @spec decode(encoded_tx) :: t
  def decode(<<0x01,
               inputs_length :: size(8),
               inputs :: binary-size(inputs_length),
               outputs_length :: size(8),
               outputs :: binary-size(outputs_length)>>) do
    decoded_inputs = for <<input :: binary-size(97) <- inputs>>, do: Input.decode(input)
    decoded_outputs = for <<output :: binary-size(36) <- outputs>>, do: Input.decode(output)
    %__MODULE__{inputs: decoded_inputs, outputs: decoded_outputs}
  end
  
  @spec coinbase(binary) :: t
  def coinbase(fingerprint) do
    %__MODULE__{outputs: [%Output{fingerprint: fingerprint, value: 50}]}
  end
  
end

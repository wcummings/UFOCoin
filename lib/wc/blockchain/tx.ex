alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output

defmodule WC.Blockchain.TX do
  @enforce_keys [:version, :inputs, :outputs]
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
  def encode(%__MODULE__{version: 1, inputs: inputs, outputs: outputs}) do
    [
      <<0x01>>,
      <<length(inputs) :: size(8)>>,
      Enum.map(inputs, &Input.encode/1),
      <<length(outputs) :: size(8)>>,      
      Enum.map(outputs, &Output.encode/1)
    ]
  end

  @spec decode(encoded_tx) :: t
  def decode(<<0x01, bin :: binary>>) do
    {bin2, inputs} = decode_inputs(bin)
    outputs = decode_outputs(bin2)
    %__MODULE__{version: 1, inputs: inputs, outputs: outputs}
  end

  #
  # PRIVATE
  #
  
  def decode_inputs(<<count :: size(8), rest :: binary>>) do
    decode_inputs(count, rest, [])
  end
  
  def decode_inputs(0, rest, acc) do
    {rest, acc}
  end
  
  def decode_inputs(count, <<input :: binary-size(97), rest :: binary>>, acc) do
    decode_inputs(count - 1, rest, [Input.decode(input)|acc])
  end

  def decode_outputs(<<count :: size(8), rest :: binary>>) do
    decode_outputs(count, rest, [])
  end
  
  def decode_outputs(0, <<>>, acc) do
    acc
  end
  
  def decode_outputs(count, <<output :: binary-size(36), rest :: binary>>, acc) do
    decode_outputs(count, rest, [Output.decode(output)|acc])
  end
  
end

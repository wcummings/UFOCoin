alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output

defmodule WC.Blockchain.TX do
  @enforce_keys [:version, :inputs, :outputs]
  defstruct version: 1, inputs: nil, outputs: nil

  @type t :: %__MODULE__{version: non_neg_integer, inputs: list(Input.t), outputs: list(Output.t)}
  @type encoded_tx :: binary

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

  def decode_inputs(<<count :: size(8), rest :: binary>>) do
    decode_inputs(count, rest, [])
  end
  
  def decode_inputs(0, rest, acc) do
    {rest, acc}
  end
  
  def decode_inputs(count, <<input :: binary - size(97), rest :: binary>>, acc) do
    decode_inputs(count - 1, rest, [Input.decode(input)|acc])
  end

  def decode_outputs(<<count :: size(8), rest :: binary>>) do
    decode_outputs(count, rest, [])
  end
  
  def decode_outputs(0, <<>>, acc) do
    acc
  end
  
  def decode_outputs(count, <<output :: binary - size(36), rest :: binary>>, acc) do
    decode_outputs(count, rest, [Output.decode(output)|acc])
  end
  
end

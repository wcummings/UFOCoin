alias OTC.Blockchain.Input, as: Input
alias OTC.Blockchain.Output, as: Output

defmodule OTC.Blockchain.TX do
  defstruct version: 1, inputs: nil, outputs: nil

  @type t :: %OTC.Blockchain.TX{version: non_neg_integer(), inputs: list(Input.t), outputs: list(Output.t)}

  def serialize(%OTC.Blockchain.TX{inputs: inputs, outputs: outputs}) do
    serialized_inputs = Enum.map(inputs, &Input.serialize/1)
    |> OTC.Util.binary_join
    serialized_outputs = Enum.map(outputs, &Output.serialize/1)
    |> OTC.Util.binary_join
    # FIXME: include version?
    OTC.Util.binary_join([serialized_inputs, serialized_outputs])    
  end
  
  def hash(tx = %OTC.Blockchain.TX{}) do
    :crypto.hash(:sha256, serialize(tx))
  end

end

alias MBC.Blockchain.Input, as: Input
alias MBC.Blockchain.Output, as: Output

defmodule MBC.Blockchain.TX do
  defstruct version: 1, inputs: nil, outputs: nil

  @type t :: %MBC.Blockchain.TX{version: non_neg_integer(), inputs: list(Input.t), outputs: list(Output.t)}

  def serialize(%MBC.Blockchain.TX{inputs: inputs, outputs: outputs}) do
    serialized_inputs = Enum.map(inputs, &Input.serialize/1)
    |> MBC.Util.binary_join
    serialized_outputs = Enum.map(outputs, &Output.serialize/1)
    |> MBC.Util.binary_join
    # FIXME: include version?
    MBC.Util.binary_join([serialized_inputs, serialized_outputs])    
  end
  
  def hash(tx = %MBC.Blockchain.TX{}) do
    :crypto.hash(:sha256, serialize(tx))
  end

end

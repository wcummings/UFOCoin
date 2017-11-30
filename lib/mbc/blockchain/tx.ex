alias MBC.Blockchain.Input, as: Input
alias MBC.Blockchain.Output, as: Output

defmodule MBC.Blockchain.TX do
  defstruct version: 1, inputs: nil, outputs: nil

  @type t :: %MBC.Blockchain.TX{version: non_neg_integer(), inputs: list(), outputs: list()}

end

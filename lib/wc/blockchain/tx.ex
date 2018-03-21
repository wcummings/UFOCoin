defmodule WC.Blockchain.TX do
  defstruct version: 1, inputs: nil, outputs: nil

  @type t :: %__MODULE__{version: non_neg_integer(), inputs: list(), outputs: list()}

end

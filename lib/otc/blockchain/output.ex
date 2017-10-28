alias OTC.Blockchain.Types, as: Types

defmodule OTC.Blockchain.Output do
  @enforce_keys [:amount, :destination]
  defstruct [:amount, :destination]
  
  @type t :: %OTC.Blockchain.Output{amount: non_neg_integer(), destination: Types.pubkey_hash}

  @spec serialize(t) :: binary()
  def serialize(%OTC.Blockchain.Output{amount: amount, destination: destination}) do
    <<amount::32>> <> destination
  end

end

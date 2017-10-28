alias MBC.Blockchain.Types, as: Types

defmodule MBC.Blockchain.Output do
  @enforce_keys [:amount, :destination]
  defstruct [:amount, :destination]
  
  @type t :: %MBC.Blockchain.Output{amount: non_neg_integer(), destination: Types.pubkey_hash}

  @spec serialize(t) :: binary()
  def serialize(%MBC.Blockchain.Output{amount: amount, destination: destination}) do
    <<amount::32>> <> destination
  end

end

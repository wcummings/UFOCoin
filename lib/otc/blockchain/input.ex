defmodule OTC.Blockchain.Input do
  @enforce_keys [:tx_hash, :txout_index, :pubkey, :signature]
  defstruct [:tx_hash, :txout_index, :pubkey, :signature]

  # FIXME: what types for pubkey and signature?
  @type t :: %OTC.Blockchain.Input{tx_hash: String.t, txout_index: non_neg_integer(), pubkey: term, signature: term}

  @spec serialize(t) :: binary()
  def serialize(%OTC.Blockchain.Input{tx_hash: tx_hash, txout_index: txout_index, pubkey: pubkey, signature: signature}) do
    <<>> # FIXME
    # <<tx_hash, txout_index, pubkey, signature>>
  end
  
end

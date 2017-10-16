defmodule OTC.P2P.Addr do
  @enforce_keys [:ip]
  defstruct [:ip, :port, :last_seen]

  @type t :: %OTC.P2P.Addr{ip: non_neg_integer, port: non_neg_integer, last_seen: non_neg_integer}
end

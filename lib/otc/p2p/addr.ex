defmodule OTC.P2P.Addr do
  @enforce_keys [:ip]
  defstruct [:ip, :port]

  @type t :: %OTC.P2P.Addr{ip: non_neg_integer, port: non_neg_integer}
end

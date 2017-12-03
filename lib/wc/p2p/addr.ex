require Logger

defmodule WC.P2P.Addr do
  @enforce_keys [:ip]
  defstruct [:ip, :port, :last_seen]

  @type encoded_addr :: binary
  @type t :: %WC.P2P.Addr{ip: tuple, port: non_neg_integer, last_seen: non_neg_integer | nil}

  @spec encode(t) :: encoded_addr
  def encode(%WC.P2P.Addr{ip: ip, port: port}) do
    <<WC.Util.ip_to_long(ip) :: size(32), port :: size(16)>>
  end

  @spec decode(encoded_addr) :: t
  def decode(<<ip :: size(32), port :: size(16)>>) do
    %WC.P2P.Addr{ip: WC.Util.long_to_ip(ip), port: port}
  end

end

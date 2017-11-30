require Logger

defmodule MBC.P2P.Addr do
  @enforce_keys [:ip]
  defstruct [:ip, :port, :last_seen]

  @type encoded_addr :: binary
  @type t :: %MBC.P2P.Addr{ip: tuple, port: non_neg_integer, last_seen: non_neg_integer}

  @spec encode(t) :: encoded_addr
  def encode(%MBC.P2P.Addr{ip: ip, port: port}) do
    <<MBC.Util.ip_to_long(ip) :: size(32), port :: size(16)>>
  end

  @spec decode(encoded_addr) :: t
  def decode(<<ip :: size(32), port :: size(16)>>) do
    %MBC.P2P.Addr{ip: MBC.Util.long_to_ip(ip), port: port}
  end

end

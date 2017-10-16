defmodule OTC do
  @moduledoc """
  Documentation for OTC.
  """

  @version "alpha broadway 0.0001"
  
  def start_link(port) do
    mnesia_tables = [OTC.P2P.AddrTable]
    Enum.each(mnesia_tables, fn (table) -> table.init end)
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OTC.P2P.Protocol, [])
  end

  def version() do
    @version
  end
  
end

defmodule OTC do
  @moduledoc """
  Documentation for OTC.
  """

  @version "alpha broadway 0.0001"
  
  def start_link(port) do
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OTC.P2PProtocol, [])
  end

  def version() do
    @version
  end
  
end

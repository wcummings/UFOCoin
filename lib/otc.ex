defmodule OTC do
  @moduledoc """
  Documentation for OTC.
  """

  def start_link(port) do
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OTC.P2PProtocol, [])
  end
  
end

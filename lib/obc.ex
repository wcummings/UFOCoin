defmodule OBC do
  @moduledoc """
  Documentation for OBC.
  """

  def start_link(port) do
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OBC.P2PProtocol, [])
  end
  
end

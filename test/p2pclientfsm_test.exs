defmodule P2PClientFSMTest do
  use ExUnit.Case
  
  test "Handshake" do
    OTC.start_link()
    # {:ok, pid} = OTC.P2P.ClientFSM.start_link('127.0.0.1', 9009)
    :timer.sleep(10000)
    # assert match?({:connected, _}, :sys.get_state(pid))
  end

end
defmodule P2PClientTest do
  use ExUnit.Case
  
  test "Heartbeats" do
    {:ok, _} = OBC.start_link(9009)
    {:ok, pid} = OBC.P2PClient.start_link('127.0.0.1', 9009)
    response = OBC.P2PClient.heartbeat(pid)
    assert response["result"] == "heartbeat"
  end

end

defmodule P2PClientTest do
  use ExUnit.Case
  
  test "Heartbeats" do
    {:ok, _} = OTC.start_link(9009)
    {:ok, pid} = OTC.P2PClient.start_link('127.0.0.1', 9009)
    for _ <- 1..3 do
      response = OTC.P2PClient.heartbeat(pid)
      assert response.result == "heartbeat"
    end
  end

end

alias MBC.P2P.HandshakeSupervisor, as: HandshakeSupervisor

defmodule MBC.P2P.Acceptor do

  def start_link(port) do
    {:ok, socket} = :gen_tcp.listen(port, [:binary, packet: 4, active: false, reuseaddr: true])    
    {:ok, spawn_link fn -> loop(socket) end}
  end

  def loop(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = HandshakeSupervisor.start_child(client)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop(socket)
  end
  
end

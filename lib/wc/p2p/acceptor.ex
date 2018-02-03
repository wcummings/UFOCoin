alias WC.P2P.HandshakeSupervisor, as: HandshakeSupervisor

defmodule WC.P2P.Acceptor do

  @accept_timeout 1000
  
  def start_link(port) do
    {:ok, socket} = :gen_tcp.listen(port, [:binary, packet: 4, active: false, reuseaddr: true])    
    {:ok, spawn_link fn -> loop(socket) end}
  end

  def loop(socket) do
    case :gen_tcp.accept(socket, @accept_timeout) do
      {:ok, client} ->
	# YOLO
	pid = spawn_link fn ->
	  receive do :tcp_ack -> :ok end
	  {:ok, pid} = HandshakeSupervisor.start_child(client)
	end
	:ok = :gen_tcp.controlling_process(client, pid)
	send pid, :tcp_ack
	loop(socket)
      {:error, :timeout} ->
	loop(socket)
    end
  end
  
end

require Logger

defmodule OTC.P2P.AddrServer do
  use GenServer

  @initial_state %{addrs_by_ref: %{}, pids_by_ref: %{}}
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def checkout() do
    GenServer.call(__MODULE__, :checkout)
  end

  def broadcast(packet) do
    GenServer.cast(__MODULE__, {:broadcast, packet})
  end
  
  def init([]) do
    {:ok, @initial_state}
  end

  def handle_call(:checkout, {from, _}, state = %{addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}) do
    addrs = OTC.P2P.AddrTable.get_addrs()
    connected_addrs = Map.values(addrs_by_ref)
    |> Enum.map(fn (%OTC.P2P.Addr{ip: ip, port: port}) -> {ip, port} end)

    my_ip = Application.get_env(:otc, :ip)
    my_port = Application.get_env(:otc, :port)
    
    eligible_addrs = Enum.filter(addrs, fn (%OTC.P2P.Addr{ip: ip, port: port}) ->
      not Enum.member?(connected_addrs, {ip, port}) and {ip, port} != {my_ip, my_port}
    end)
    
    if length(eligible_addrs) > 0 do
      [addr] = Enum.take_random(eligible_addrs, 1)
      ref = Process.monitor(from)
      addrs_by_ref = Map.put(addrs_by_ref, ref, addr)
      pids_by_ref = Map.put(pids_by_ref, ref, from)
      Logger.info "Checked out #{addr.ip}:#{addr.port}"
      {:reply, {:ok, addr}, %{state | addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}}
    else
      {:reply, {:error, :exhausted}, state}
    end
  end

  def handle_cast({:broadcast, packet}, state = %{pids_by_ref: pids_by_ref}) do
    for pid <- Map.values(pids_by_ref), do: GenServer.cast(pid, {:broadcast, packet})
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, _, _, _}, state = %{addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}) do
    addr = addrs_by_ref[ref]
    addrs_by_ref = Map.delete(addrs_by_ref, ref)
    pids_by_ref = Map.delete(pids_by_ref, ref)
    Logger.info "Released #{addr.ip}:#{addr.port}"
    {:noreply, %{state | addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}}
  end

end

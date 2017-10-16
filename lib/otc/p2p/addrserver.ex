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

  def handle_call(:checkout, from, state = %{addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}) do
    addrs = OTC.P2P.AddrTable.get_addrs()
    connected_addrs = Map.values(addrs_by_ref)
    eligible_addrs = Enum.filter(addrs, fn addr -> Enum.member?(connected_addrs, addr) end)
    if length(eligible_addrs) > 0 do
      [addr] = Enum.take_random(eligible_addrs, 1)
      ref = Process.monitor(from)
      addrs_by_ref = Map.put(addrs_by_ref, ref, addr)
      pids_by_ref = Map.put(pids_by_ref, ref, from)
      {:reply, {:ok, addr}, %{state | addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}}
    else
      {:reply, {:error, :exhausted}, state}
    end
  end

  def handle_cast({:broadcast, packet}, state = %{pids_by_ref: pids_by_ref}) do
    for pid <- Map.values(pids_by_ref), do: GenServer.cast(pid, packet)
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, _, _, _}, state = %{addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}) do
    addrs_by_ref = Map.delete(addrs_by_ref, ref)
    pids_by_ref = Map.delete(pids_by_ref, ref)
    {:noreply, %{state | addrs_by_ref: addrs_by_ref, pids_by_ref: pids_by_ref}}
  end

end

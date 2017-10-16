require Logger

defmodule OTC.P2P.AddrTable do

  def init() do
    :mnesia.create_table(Addr, [attributes: [:host, :last_seen]])
  end

  def add_addr(%OTC.P2P.Addr{ip: ip, port: port}) do
    Logger.info "Writing addr to mnesia: #{ip}:#{port}"
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.write({Addr, {ip, port}, :os.system_time(:millisecond)}) end)
    result
  end

  @spec add_addrs(list(OTC.P2P.Addr.t)) :: :ok | {:error, term}
  def add_addrs(addrs) do
    {:atomic, result} = :mnesia.transaction(fn ->
      Enum.each(addrs, fn (%OTC.P2P.Addr{ip: ip, port: port}) ->
	Logger.info "Writing addr to mnesia: #{ip}:#{port}"	
	:mnesia.write({Addr, {ip, port}, :os.system_time(:millisecond)})
      end)
    end)
    result
  end

  def get_addrs() do
    {:atomic, addrs} = :mnesia.transaction(fn -> :mnesia.match_object({Addr, :_, :_}) end)
    Enum.map(addrs, fn ({Addr, {ip, port}, _last_seen}) ->
      %OTC.P2P.Addr{ip: ip, port: port}
    end)
  end

end

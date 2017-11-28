require Logger

alias MBC.P2P.Addr, as: P2PAddr

defmodule MBC.P2P.AddrTable do

  # FIXME: Addr -> :p2paddr or P2PAddrTable or something like that
  
  def init do
    :mnesia.create_table(Addr, [attributes: [:host, :last_seen]])
  end

  def insert(addr) when is_map(addr) do
    insert([addr])
  end

  @spec insert(list(P2PAddr.t)) :: :ok | {:error, term}
  def insert(addrs) when is_list(addrs) do
    {:atomic, result} = :mnesia.transaction(fn ->
      Enum.each(addrs, fn (addr = %P2PAddr{ip: ip, port: port}) ->
	if get(addr) == {:error, :notfound} do
	  Logger.debug "New peer discovered: #{inspect(ip)}:#{port}"	
	end
	:mnesia.write({Addr, {ip, port}, :os.system_time(:millisecond)})	
      end)
    end)
    result
  end

  def get_all() do
    {:atomic, addrs} = :mnesia.transaction(fn -> :mnesia.match_object({Addr, :_, :_}) end)
    Enum.map(addrs, fn ({Addr, {ip, port}, last_seen}) ->
      %P2PAddr{ip: ip, port: port, last_seen: last_seen}
    end)
  end

  def get(%P2PAddr{ip: ip, port: port}) do
    case :mnesia.transaction(fn -> :mnesia.read(Addr, {ip, port}) end) do
      {:atomic, [{Addr, {^ip, ^port}, last_seen}]} ->
	{:ok, %P2PAddr{ip: ip, port: port, last_seen: last_seen}}
      {:atomic, []} ->
	{:error, :notfound}
    end
  end
  
end

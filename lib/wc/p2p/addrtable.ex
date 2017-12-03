require Logger

alias WC.P2P.Addr, as: P2PAddr

defmodule WC.P2P.AddrTable do

  # FIXME: Addr -> :p2paddr or P2PAddrTable or something like that
  
  def init do
    :mnesia.create_table(P2PAddrTable, [attributes: [:host, :last_seen]])
  end

  @spec insert(P2PAddr.t) :: :ok | {:error, term}
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
	:mnesia.write({P2PAddrTable, {ip, port}, :os.system_time(:millisecond)})	
      end)
    end)
    result
  end

  @spec get_all() :: list(P2PAddr.t)
  def get_all do
    {:atomic, addrs} = :mnesia.transaction(fn -> :mnesia.match_object({P2PAddrTable, :_, :_}) end)
    Enum.map(addrs, fn ({P2PAddrTable, {ip, port}, last_seen}) ->
      %P2PAddr{ip: ip, port: port, last_seen: last_seen}
    end)
  end

  @spec get(P2PAddr.t) :: {:ok, P2PAddr.t} | {:error, :notfound}
  def get(%P2PAddr{ip: ip, port: port}) do
    case :mnesia.transaction(fn -> :mnesia.read(P2PAddrTable, {ip, port}) end) do
      {:atomic, [{P2PAddrTable, {^ip, ^port}, last_seen}]} ->
	{:ok, %P2PAddr{ip: ip, port: port, last_seen: last_seen}}
      {:atomic, []} ->
	{:error, :notfound}
    end
  end
  
end

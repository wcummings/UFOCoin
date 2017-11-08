require Logger

defmodule MBC do
  @genesis_block %MBC.Blockchain.Block{prev_block_hash: <<0 :: size(32)>>, difficulty: 1, timestamp: :os.system_time(:millisecond)}

  @version "alpha broadway tango"

  def genesis_block do
    @genesis_block
  end
  
  def start(_, _) do
    Logger.info "Starting MBC"

    Logger.info "Initializing mnesia tables..."
    mnesia_tables = [
      MBC.P2P.AddrTable,
      MBC.KeyTable,
      MBC.Blockchain.BlockTable,
      MBC.Blockchain.MempoolTable
    ]
    Enum.each(mnesia_tables, fn table -> table.init end)

    # Insert genesis block
    Logger.info "Inserting genesis block: #{inspect(@genesis_block)}"
    MBC.Blockchain.BlockTable.insert(@genesis_block)
    
    # Insert seed nodes
    seed_nodes = lookup_seed_nodes()
    default_port = Application.get_env(:mbc, :default_port)

    Enum.each(seed_nodes, fn ip ->
      MBC.P2P.AddrTable.insert(%MBC.P2P.Addr{ip: ip, port: default_port})
    end)

    if Application.get_env(:mbc, :detect_ip) do
      local_ip = get_local_ipv4_address()
      Logger.info "detect_ip enabled, IP address detected as #{inspect(local_ip)}"
      Application.put_env(:mbc, :ip, local_ip)
    end
    
    {:ok, _} = MBC.Supervisor.start_link
    outbound_connections = Application.get_env(:mbc, :outbound_connections)
    for _ <- 1 .. outbound_connections, do: {:ok, _} = MBC.P2P.ClientFSMSupervisor.start_client

    port = Application.get_env(:mbc, :port)
    Logger.info "Listening on #{port}"
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], MBC.P2P.Protocol, [])
  end

  def lookup_seed_nodes do
    seed_dns = Application.get_env(:mbc, :seed_dns)
    {:ok, {:hostent, ^seed_dns, _, :inet, _, ips}} = :inet_res.getbyname(seed_dns, :a)
    ips
  end

  def get_local_ipv4_address do
    {:ok, addrs} = :inet.getifaddrs()
    Enum.map(addrs, fn {_dev, opts} -> Keyword.get(opts, :addr) end)
    |> Enum.filter(fn addr -> addr != nil and Kernel.tuple_size(addr) == 4 and addr != {127, 0, 0, 1} end)
    |> hd
  end

  def version do
    @version
  end
  
end

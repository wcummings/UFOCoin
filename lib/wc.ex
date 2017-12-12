require Logger

alias WC.Blockchain.Block, as: Block
alias WC.Blockchain.BlockHeader, as: BlockHeader

defmodule WC do
  @genesis_block %Block{header: %BlockHeader{prev_block_hash: <<0 :: size(256)>>, difficulty: 1, height: 0, timestamp: 0}, txs: []}
  @version "ALPHA"
  
  def genesis_block do
    @genesis_block
  end

  def version do
    @version
  end
  
  def start(_, _) do
    Logger.info "Starting WhipCash node"

    Logger.info "Initializing mnesia tables..."
    mnesia_tables = [
      WC.P2P.AddrTable,
      WC.Blockchain.BlockHashIndex,
      WC.Blockchain.OrphanBlockTable
    ]
    
    Enum.each(mnesia_tables, fn table ->
      case table.init do
	{:aborted, {:already_exists, _}} ->
	  :ok
	{:aborted, error} ->
	  Logger.error "Error setting up mnesia table: #{inspect(error)}"
	  exit(error)
	{:atomic, :ok} ->
	  :ok
      end
    end)

    # Insert seed nodes
    seed_nodes = lookup_seed_nodes()
    default_port = Application.get_env(:wc, :default_port)

    for ip <- seed_nodes, do: WC.P2P.AddrTable.insert(%WC.P2P.Addr{ip: ip, port: default_port})

    if Application.get_env(:wc, :detect_ip) do
      local_ip = get_local_ipv4_address()
      Logger.info "IP address detected as #{inspect(local_ip)}"
      Application.put_env(:wc, :ip, local_ip)
    end
    
    {:ok, pid} = WC.Supervisor.start_link
    outbound_connections = Application.get_env(:wc, :outbound_connections)
    for _ <- 1 .. outbound_connections, do: {:ok, _} = WC.P2P.ClientFSMSupervisor.start_client
    {:ok, pid}
  end

  def lookup_seed_nodes do
    seed_dns = Application.get_env(:wc, :seed_dns)
    {:ok, {:hostent, ^seed_dns, _, :inet, _, ips}} = :inet_res.getbyname(seed_dns, :a)
    ips
  end

  def get_local_ipv4_address do
    {:ok, addrs} = :inet.getifaddrs()
    Enum.map(addrs, fn {_dev, opts} -> Keyword.get(opts, :addr) end)
    |> Enum.filter(fn addr -> addr != nil and Kernel.tuple_size(addr) == 4 and addr != {127, 0, 0, 1} end)
    |> hd
  end
    
end

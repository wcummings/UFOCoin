require Logger

alias WC.Blockchain.OrphanBlockTable, as: OrphanBlockTable
alias WC.P2P.AddrTable, as: P2PAddrTable
alias WC.Wallet.KeyStore, as: KeyStore
alias WC.P2P.UPnP, as: UPnP

defmodule WC do
  @version "ALPHA"

  def version do
    @version
  end
  
  def start(_, _) do
    Logger.info "Starting WhipCash node"

    print_configuration()
    
    Logger.debug "Initializing mnesia tables..."
    mnesia_tables = [
      P2PAddrTable,
      OrphanBlockTable,
      KeyStore
    ]

    Enum.each(mnesia_tables, fn table ->
      Logger.debug "Initializing #{inspect(table)}..."
      case table.init do
	{:aborted, {:already_exists, _}} ->
	  :ok
	{:aborted, error} ->
	  Logger.error "Error setting up mnesia table: #{inspect(error)}"
	  exit(:error)
	{:atomic, :ok} ->
	  :ok
      end
    end)

    setup_keystore()
    
    # Insert seed nodes
    seed_nodes = lookup_seed_nodes()
    default_port = Application.get_env(:wc, :default_port)

    for ip <- seed_nodes, do: WC.P2P.AddrTable.insert(%WC.P2P.Addr{ip: ip, port: default_port})

    Application.put_env(:wc, :ip, get_ip())
    
    {:ok, pid} = WC.Supervisor.start_link
    port = Application.get_env(:wc, :port, default_port)
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], WC.P2P.Handshake, [])

    ip = Application.get_env(:wc, :ip)
    Logger.info "Listening on #{inspect(ip)}"

    {:ok, pid}
  end

  def setup_keystore do
    if length(KeyStore.get_all_keypairs()) == 0 do
      fingerprint = KeyStore.generate_key
      # address = Base58Check.encode58check(128, fingerprint)
      address = KeyStore.encode_fingerprint(fingerprint)
      Logger.info "KeyStore empty, generated key: #{address}"
      Application.put_env(:wc, :coinbase_address, address)
    end

    coinbase_address = Application.get_env(:wc, :coinbase_address)
    
    if coinbase_address == nil do
      Logger.error("coinbase_address must be set")
      exit(:error)
    else
      try do
	Base58Check.decode58check(coinbase_address)
      rescue
	ArgumentError ->
	  Logger.error("coinbase_address is invalid: #{coinbase_address}")
	  exit(:error)
      end
    end
  end
  
  def lookup_seed_nodes do
    seed_dns = Application.get_env(:wc, :seed_dns)
    {:ok, {:hostent, ^seed_dns, _, :inet, _, ips}} = :inet_res.getbyname(seed_dns, :a)
    ips
  end

  def get_local_ipv4_address do
    {:ok, addrs} = :inet.getifaddrs
    Enum.map(addrs, fn {_dev, opts} -> Keyword.get(opts, :addr) end)
    |> Enum.filter(fn addr -> addr != nil and Kernel.tuple_size(addr) == 4 and addr != {127, 0, 0, 1} end)
    |> hd
  end

  # Possibly a better method?
  # get_myip(Ip) ->
  #   [{_, {MyIp, _}}|_] = inet_ext:route(Ip),
  #   inet_parse:ntoa(MyIp).

  def print_configuration do
    Logger.info "Configuration:"
    Enum.filter(Application.get_all_env(:wc), fn {key, _} -> key != :included_applications end)
    |> Enum.each(fn {key, value} -> Logger.info "#{key} => #{inspect(value)}" end)
    Logger.info "End Configuration."
  end

  def get_ip do
    case Application.get_env(:wc, :ip) do
      :local ->
	:ok
	get_local_ipv4_address()
      {:ip, ip} ->
	ip
      {:nat, external_port} ->
	case UPnP.get_ip(Application.get_env(:wc, :port), external_port, 0) do
	  {:ok, ip, _nat_context} ->
	    ip
	  _ ->
	    ip = get_local_ipv4_address()
	    ip
	end
    end
  end
  
end

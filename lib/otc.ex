require Logger

defmodule OTC do
  @moduledoc """
  Documentation for OTC.
  """

  @version "alpha broadway tango"
  
  def start(_, _) do
    Logger.info "Starting the One True Chain"
    port = Application.get_env(:otc, :port)
    Logger.info "Listening on #{port}"
    mnesia_tables = [OTC.P2P.AddrTable]
    Enum.each(mnesia_tables, fn (table) -> table.init end)
    # Insert seed nodes
    # seed_nodes = Application.get_env(:otc, :seed_nodes)
    seed_nodes = lookup_seed_nodes()
    |> Enum.map(&:erlang.list_to_binary/1)
    default_port = Application.get_env(:otc, :default_port)
    Enum.each(seed_nodes, fn (ip) ->
      OTC.P2P.AddrTable.add_addr(%OTC.P2P.Addr{ip: ip, port: default_port})
    end)
    {:ok, _} = OTC.Supervisor.start_link
    outbound_connections = Application.get_env(:otc, :outbound_connections)
    for _ <- 1 .. outbound_connections, do: {:ok, _} = OTC.P2P.ClientFSMSupervisor.start_client
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OTC.P2P.Protocol, [])
  end

  def lookup_seed_nodes do
    seed_dns = Application.get_env(:otc, :seed_dns)
    {:ok, {:hostent, ^seed_dns, _, :inet, _, ips}} = :inet_res.getbyname(seed_dns, :a)
    Enum.map(ips, &:inet_parse.ntoa/1)
  end
  
  def version do
    @version
  end
  
end

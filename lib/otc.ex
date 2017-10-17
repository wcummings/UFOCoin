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
    seed_nodes = Application.get_env(:otc, :seed_nodes)
    Enum.each(seed_nodes, fn ({ip, port}) ->
      OTC.P2P.AddrTable.add_addr(%OTC.P2P.Addr{ip: ip, port: port})
    end)
    {:ok, _} = OTC.Supervisor.start_link
    outbound_connections = Application.get_env(:otc, :outbound_connections)
    for _ <- 1 .. outbound_connections, do: {:ok, _} = OTC.P2P.ClientFSMSupervisor.start_client
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], OTC.P2P.Protocol, [])
  end

  def version() do
    @version
  end
  
end

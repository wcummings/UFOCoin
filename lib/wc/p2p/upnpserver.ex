require Logger

defmodule WC.P2P.UPnPServer do
  use GenServer

  def start_link(context, internal_port, external_port, timeout) do
    GenServer.start_link(__MODULE__, [context, internal_port, external_port, timeout], name: __MODULE__)
  end

  def init([context, external_port, internal_port, timeout]) do
    {:ok, %{context: context, internal_port: internal_port, external_port: external_port, timeout: timeout}, 0}
  end

  def handle_info(:timeout, state = %{context: context, internal_port: internal_port, external_port: external_port, timeout: timeout}) do
    Logger.info "Renewing UPnP lease..."
    :ok = :nat_upnp.add_port_mapping(context, :tcp, external_port, internal_port, nat_description(), timeout)
    {:noreply, state, div(timeout, 2)}
  end

  def get_ip(internal_port, external_port) do
    if Application.get_env(:wc, :enable_nat) do
      case :nat_upnp.discover do
	{:ok, context} ->
	  case :nat_upnp.add_port_mapping(context, :tcp, external_port, internal_port, nat_description(), 0) do
	    :ok ->
	      {:ok, ip_address} = :nat_upnp.get_external_ip_address(context)
	      {:ok, ip_address, context}
	    error ->
	      error
	  end
      end
    else
      {:error, :nat_disabled}
    end
  end

  def nat_description do
    :erlang.binary_to_list("WhipCash" <> WC.version)
  end
  
end

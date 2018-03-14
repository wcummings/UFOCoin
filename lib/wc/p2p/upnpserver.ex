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
    :nat.add_port_mapping(context, :tcp, internal_port, external_port, timeout)
    {:noreply, state, div(timeout * 1000, 2)}
  end

  def get_ip(internal_port, external_port, timeout) do
    # TODO: An error monad would really clean this up
    case :nat.discover do
      {:ok, context} ->
	case :nat.add_port_mapping(context, :tcp, internal_port, external_port, timeout) do
	  :ok ->
	    case :nat.get_external_address(context) do
	      {:ok, ip_address} ->
		{:ok, ip_address, context}
	      error ->
		error
	    end
	  error ->
	    error
	end
      error ->
	error
    end
  end

end

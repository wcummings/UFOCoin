require Logger

defmodule WC.P2P.UPnP do

  def get_ip(internal_port, external_port, timeout) do
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

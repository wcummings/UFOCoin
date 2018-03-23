defmodule WC.P2P.ConnectionRegistry do

  @spec register(String.t) :: :ok
  def register(topic) do
    {:ok, _} = Registry.register(:connection_registry, topic, [])
    :ok
  end

  @spec broadcast(String.t, any) :: :ok
  def broadcast(topic, message) do
    broadcast(topic, message, [])
  end

  @spec broadcast(String.t, String.t, list(pid)) :: :ok
  def broadcast(topic, message, excluded_pids) do
    Registry.dispatch(:connection_registry, topic, fn entries ->
      Enum.filter(entries, fn {pid, _} -> not :lists.member(pid, excluded_pids) end)
      |> Enum.each(fn {pid, _} -> send pid, {:connection_registry, topic, message} end)
    end)
  end

end

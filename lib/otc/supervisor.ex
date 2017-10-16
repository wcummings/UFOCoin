defmodule OTC.Supervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], [])
  end

  def init([]) do
    children = [
      {OTC.P2P.ClientFSMSupervisor, name: OTC.P2P.ClientFSMSupervisor},
      {OTC.P2P.AddrServer, name: OTC.P2P.AddrServer}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  
end

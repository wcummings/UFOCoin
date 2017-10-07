use Amnesia

defdatabase Database do

  deftable Peer, [:ip, :last_seen], type: :bag do
    @type t :: %Peer{ip: Socket.Address.t, last_seen: nil}

    def add_peer(ip) do
      %Peer{ip: ip} |> Peer.write
    end

    def get_peers() do
      Peer.read('_')
    end
  end

end

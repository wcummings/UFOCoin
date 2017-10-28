defmodule MBC.KeyTable do

  @rsa_public_exponent 65537
  @rsa_key_size 256
  
  def init() do
    :mnesia.create_table(Key, [attributes: [:pubkeyhash, :pubkey, :privkey]])
  end

  def insert(pubkeyhash, pubkey, privkey) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.write({Key, pubkeyhash, {pubkey, privkey}}) end)
    :ok
  end

  def get_keypair_by_pubkey_hash(pubkey_hash) do
    case :mnesia.transaction(fn -> :mnesia.read({Key, pubkey_hash}) end) do
      {:atomic, [{Key, ^pubkey_hash, {pubkey, privkey}}]} ->
	{:ok, {pubkey, privkey}}
      {:atomic, []} ->
	{:error, :nosuchkeypair}
    end
  end

  def get_all_pubkey_hashes do
    {:atomic, rows} = :mnesia.transaction(fn -> :mnesia.match_object({Key, :_, :_}) end)
    Enum.map(rows, fn {Key, pubkey_hash, _} -> pubkey_hash end)
  end

  def generate_keypair do
    {pubkey, privkey} = :crypto.generate_key(:rsa, {@rsa_key_size, @rsa_public_exponent})
    pubkey_hash = :crypto.hash(:sha256, pubkey)
    :ok = insert(pubkey_hash, pubkey, privkey)
    {pubkey_hash, pubkey, privkey}
  end
  
end

defmodule OTC.KeyTable do

  @rsa_public_exponent 65537
  @rsa_key_size 256
  
  def init() do
    :mnesia.create_table(Key, [attributes: [:pubkeyhash, :pubkey, :privkey]])
  end

  def insert(pubkeyhash, pubkey, privkey) do
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.write({Key, pubkeyhash, {pubkey, privkey}}) end)
    :ok
  end

  def get_keypair_by_pubkeyhash(pubkeyhash) do
    case :mnesia.transaction(fn -> :mnesia.read({Key, pubkeyhash}) end) do
      {:atomic, [{Key, ^pubkeyhash, {pubkey, privkey}}]} ->
	{:ok, {pubkey, privkey}}
      {:atomic, []} ->
	{:error, :nosuchkeypair}
    end
  end

  def get_all_pubkeyhashes do
    {:atomic, rows} = :mnesia.transaction(fn -> :mnesia.match_object({Key, '_', '_'}) end)
    Enum.map(rows, fn {Key, pubkeyhash, _} -> pubkeyhash end)
  end

  def generate_keypair do
    {pubkey, privkey} = :crypto.generate_key(:rsa, {@rsa_key_size, @rsa_public_exponent})
    pubkeyhash = :crypto.hash(:sha256, pubkey)
    :ok = insert(pubkeyhash, pubkey, privkey)
    {pubkeyhash, pubkey, privkey}
  end
  
end

defmodule WC.Wallet.KeyStore do

  @public_exponent 5
  @modulus_size 256

  @type fingerprint :: binary()
  @type pubkey :: binary()
  @type privkey :: binary()
  @type keypair :: {pubkey, privkey}
    
  def init do
    :mnesia.create_table(KeyStoreTable, [attributes: [:fingerprint, :keypair], type: :set, disc_copies: [Node.self()]])
    # :mnesia.create_table(KeyStoreTable, [attributes: [:fingerprint, :keypair], type: :set])
  end

  @spec generate_key :: fingerprint
  def generate_key do
    {pubkey, privkey} = :crypto.generate_key(:rsa, {@modulus_size * 8, @public_exponent})
    fingerprint = fingerprint(pubkey)
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.write({KeyStoreTable, fingerprint, {pubkey, privkey}}) end)
    fingerprint
  end

  @spec get_keypair(fingerprint) :: {:ok, keypair} | {:error, :notfound}
  def get_keypair(fingerprint) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(KeyStoreTable, fingerprint) end)
    case result do
      [{KeyStoreTable, ^fingerprint, keypair}] ->
	{:ok, keypair}
      [] ->
	{:error, :notfound}
    end
  end

  @spec get_all_keypairs :: list({fingerprint, keypair})
  def get_all_keypairs do
    {:atomic, result} = :mnesia.transaction(fn ->
      :mnesia.foldl(fn ({KeyStoreTable, fingerprint, keypair}, acc) ->
	[{fingerprint, keypair}|acc]
      end, [], KeyStoreTable)
    end)
    result
  end
  
  @spec fingerprint(pubkey) :: fingerprint
  def fingerprint(pubkey) do
    base64_pubkey = :erlang.iolist_to_binary(pubkey) |> Base.encode64()
    :crypto.hash(:sha256, base64_pubkey)
  end

end

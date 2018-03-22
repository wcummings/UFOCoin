defmodule WC.Wallet.KeyStore do

  @public_exponent 5
  @modulus_size 256

  @type keypair :: {binary, binary}

  # FIXME: should have auto inc ID
  
  def init do
    :mnesia.create_table(KeyStoreTable, [attributes: [:fingerprint, :keypair], type: :set])#, disc_copies: [Node.self()]])
  end

  def generate_key do
    {pubkey, privkey} = :crypto.generate_key(:rsa, {@modulus_size * 8, @public_exponent})
    fingerprint = fingerprint(pubkey)
    {:atomic, :ok} = :mnesia.transaction(fn -> :mnesia.write({KeyStoreTable, fingerprint, {pubkey, privkey}}) end)
    fingerprint
  end

  def get_keypair(fingerprint) do
    {:atomic, result} = :mnesia.transaction(fn -> :mnesia.read(KeyStoreTable, fingerprint) end)
    case result do
      [{KeyStoreTable, ^fingerprint, keypair}] ->
	{:ok, keypair}
      [] ->
	{:error, :notfound}
    end
  end

  def get_all_keypairs do
    {:atomic, result} = :mnesia.transaction(fn ->
      :mnesia.foldl(fn ({KeyStoreTable, fingerprint, keypair}, acc) ->
	[{fingerprint, keypair}|acc]
      end, [], KeyStoreTable)
    end)
    result
  end
  
  def fingerprint(pubkey) do
    base64_pubkey = :erlang.iolist_to_binary(pubkey) |> Base.encode64()
    :crypto.hash(:sha256, base64_pubkey)
  end

  def address(pubkey) do
    fingerprint(pubkey) |> Base58Check.encode58check(128)
  end
  
end

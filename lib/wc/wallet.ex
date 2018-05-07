alias WC.Wallet.KeyStore, as: KeyStore
alias WC.Blockchain.Output, as: Output
alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.UTXODb, as: UTXODb
alias WC.Blockchain.TX, as: TX

defmodule WC.Wallet do

  @type address :: String.t
  
  @doc "Show the UTXO balance for each keypair."
  @spec show_balances :: nil
  def show_balances do
    Enum.map(KeyStore.get_all_keypairs(), fn {fingerprint, _} ->
      %{fingerprint: Base58Check.encode58check(128, fingerprint), balance: get_balance(fingerprint)}
    end)
    |> Scribe.print
    nil
  end

  @doc "Show unspent outputs for each keypair."
  @spec show_utxo :: nil
  def show_utxo do
    Enum.map(KeyStore.get_all_keypairs(), fn {fingerprint, _} -> fingerprint end)
    |> Enum.flat_map(fn fingerprint ->
      Enum.map(UTXODb.scan(fingerprint), fn {{tx_hash, offset}, output} ->
	%{fingerprint: Base58Check.encode58check(128, fingerprint),
	  tx_hash: Base.encode16(tx_hash),
	  offset: offset,
	  value: output.value}
      end)
    end)
    |> Scribe.print
    nil
  end

  @spec make_tx(list(address), list(Output.t), non_neg_integer) :: {:ok, TX.t} | {:error, :_}
  def make_tx(input_addresses, outputs, fee) do
    total_output_value = Enum.map(outputs, fn output -> output.value end) |> Enum.sum
    valid_utxo = Enum.map(input_addresses, &decode_address/1) |> Enum.flat_map(&UTXODb.scan/1)
    case find_utxo_for_value(valid_utxo, total_output_value + fee) do
      {:ok, {utxo_to_be_spent, extra_value}} ->
	# Make output and key for change transaction
	change = extra_value - fee
	change_fingerprint = KeyStore.generate_key
	change_output = %Output{fingerprint: change_fingerprint, value: change}
	# Make inputs
	inputs = Enum.map(utxo_to_be_spent, fn {key, output} ->
	  {:ok, {pubkey, _}} = KeyStore.get_keypair(output.fingerprint)
	  {key, pubkey}
	end)
	|> Enum.map(fn {utxo_record, pubkey} -> UTXODb.utxo_record_to_input(utxo_record, pubkey) end)
	{:ok, make_tx_raw(inputs, [change_output|outputs])}
      error ->
	error
    end
  end
  
  @spec make_tx_raw(list(Input.t), list(Output.t)) :: TX.t
  def make_tx_raw(inputs, outputs) do
    tx = %TX{inputs: inputs, outputs: outputs}
    # Get private keys so we can sign the TX
    privkeys_by_fingerprint = Enum.map(inputs, fn input -> input.pubkey end)
    |> KeyStore.fingerprint()
    |> KeyStore.get_keypair()
    |> Enum.reduce(fn {fingerprint, {_, privkey}, acc} -> Map.put(acc, fingerprint, privkey) end, %{})
    TX.sign(privkeys_by_fingerprint, tx)
  end

  @spec make_output(address, non_neg_integer) :: Output.t
  def make_output(address, value) do
    %Output{fingerprint: decode_address(address), value: value}
  end

  @spec make_input(String.t, non_neg_integer, KeyStore.pubkey) :: Input.t
  def make_input(tx_hash, offset, pubkey) do
    %Input{tx_hash: Base.decode16(tx_hash), offset: offset, pubkey: pubkey}
  end

  @spec encode_address(KeyStore.fingerprint) :: address
  def encode_address(fingerprint) do
    Base58Check.encode58check(128, fingerprint)
  end

  @spec decode_address(address) :: KeyStore.fingerprint
  def decode_address(address) do
    {_, fingerprint} = Base58Check.decode58check(address)
    fingerprint
  end
  
  #
  # PRIVATE
  #

  @spec find_utxo_for_value(list(UTXOSet.utxo_record), non_neg_integer) :: {:ok, {list(UTXOSet.utxo_record), non_neg_integer}} | {:error, :not_enough_value}
  def find_utxo_for_value(sorted_utxo, total_output_value) do
    find_utxo_for_value(sorted_utxo, total_output_value, [])
  end
  
  def find_utxo_for_value([{tx_hash_and_offset, output}|sorted_utxo], total_output_value, utxo_to_be_spent) do
    find_utxo_for_value(sorted_utxo, total_output_value - output.value, [tx_hash_and_offset|utxo_to_be_spent])
  end

  def find_utxo_for_value(_, total_output_value, utxo_to_be_spent) when total_output_value <= 0 do
    # Return change
    {:ok, {utxo_to_be_spent, -total_output_value}}
  end

  def find_utxo_for_value([], total_utxo_value, _) when total_utxo_value > 0 do
    {:error, :not_enough_value}
  end

  @spec get_balance(KeyStore.fingerprint) :: non_neg_integer
  def get_balance(fingerprint) do
    UTXODb.scan(fingerprint)
    |> Enum.map(fn {_, %Output{value: value}} -> value end)
    |> Enum.sum
  end

end

alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output
alias WC.Wallet.KeyStore, as: KeyStore

require Logger

defmodule WC.Blockchain.TX do
  defstruct version: 1, inputs: [], outputs: []

  @reward 5000
  
  @type t :: %__MODULE__{version: non_neg_integer, inputs: list(Input.t), outputs: list(Output.t)}
  @type encoded_tx :: iodata
  @type tx_hash :: binary
  @type validation_error :: :negative_fee | {:bad_inputs, list(Input.t)}

  @spec sign(map(), t) :: t
  def sign(private_keys, %__MODULE__{version: 1, inputs: inputs} = tx) do
    tx_without_signatures = encode_tx_without_signatures(tx)
    inputs_with_signatures = Enum.map(inputs, fn input -> Input.sign(input, tx_without_signatures, private_keys[KeyStore.fingerprint(input.pubkey)]) end)
    %{tx | inputs: inputs_with_signatures}
  end

  @doc """
  Verifies a TX using a list of outputs corresponding to its inputs.
  referenced_outputs must already be validated, and correctly ordered
  for this to produce trustworthy results,
  """
  @spec verify(t, list(Output.t)) :: {:error, validation_error} | :ok
  def verify(tx = %__MODULE__{inputs: inputs, outputs: outputs}, referenced_outputs) do
    fee = get_fee(outputs, referenced_outputs)
    if fee < 0 do
      {:error, :negative_fee}
    else
      invalid_inputs = Enum.with_index(inputs)
      |> Enum.map(fn {input, i} -> {referenced_outputs[i], input} end)
      |> Enum.map(fn {input, output} -> {input, Input.validate(input, output, encode(tx))} end)
      |> Enum.filter(fn {_, is_valid} -> is_valid end)
      if length(invalid_inputs) > 0 do
	{:error, {:bad_inputs, invalid_inputs}}
      else
	:ok
      end
    end
  end

  @spec hash(t) :: tx_hash
  def hash(%__MODULE__{} = tx) do
    encode(tx) |> hash
  end

  @spec hash(encoded_tx) :: tx_hash
  def hash(encoded_tx) do
    :crypto.hash(:sha256, encoded_tx)
  end
  
  @spec encode(t) :: encoded_tx
  def encode(%__MODULE__{inputs: inputs, outputs: outputs}) do
    encoded_inputs = Enum.map(inputs, &Input.encode/1)
    encoded_outputs = Enum.map(outputs, &Output.encode/1) 
    [
      <<0x01>>,
      <<:erlang.iolist_size(encoded_inputs) :: size(32)>>,
      encoded_inputs,
      <<:erlang.iolist_size(encoded_outputs) :: size(32)>>,
      encoded_outputs
    ]
  end

  @spec decode(binary) :: list(t)
  def decode(tx_array) do
    decode(tx_array, [])
  end
  
  def decode(<<0x01,
               inputs_length :: size(32),
               inputs :: binary-size(inputs_length),
               outputs_length :: size(32),
               outputs :: binary-size(outputs_length), rest :: binary>>, acc) do
    decoded_inputs = for <<input :: binary-size(321) <- inputs>>, do: Input.decode(input)
    decoded_outputs = for <<output :: binary-size(36) <- outputs>>, do: Output.decode(output)
    decode(rest, [%__MODULE__{inputs: decoded_inputs, outputs: decoded_outputs}|acc])
  end

  def decode(<<>>, acc) do
    acc
  end

  @spec coinbase :: t
  def coinbase do
    # We need to make a unique input to avoid duplicate coinbase tx's in different blocks
    input = %Input{tx_hash: <<0 :: size(256)>>,
		   offset: 0,
		   pubkey: <<0 :: size(256)>>,
		   signature: :crypto.strong_rand_bytes(256)}
    {_, fingerprint} = Base58Check.decode58check(Application.get_env(:wc, :coinbase_address))
    %__MODULE__{inputs: [input], outputs: [%Output{fingerprint: fingerprint, value: @reward}]}
  end

  @spec is_coinbase?(t) :: true | false
  def is_coinbase?(%__MODULE__{outputs: [%Output{value: @reward}]}) do
    true
  end

  def is_coinbase?(_) do
    false
  end

  @spec encode_tx_without_signatures(t) :: encoded_tx
  def encode_tx_without_signatures(tx = %__MODULE__{inputs: inputs}) do
    # Set all input signatures to 0 to create the message we're going to sign
    inputs2 = Enum.map(inputs, fn input -> %{input | signature: <<0 :: size(2048)>>} end)
    # Encode full tx, w/ zero'd signatures
    encode(%{tx | inputs: inputs2})
  end
  
  @spec get_fee(list(Output.t), list(Output.t)) :: integer
  def get_fee(outputs, referenced_outputs) do
    output_value = Enum.map(outputs, fn output -> output.value end) |> Enum.sum
    input_value = Enum.map(referenced_outputs, fn output -> output.value end) |> Enum.sum
    input_value - output_value
  end

end

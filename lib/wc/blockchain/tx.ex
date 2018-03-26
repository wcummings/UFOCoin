alias WC.Blockchain.Input, as: Input
alias WC.Blockchain.Output, as: Output

require Logger

defmodule WC.Blockchain.TX do
  defstruct version: 1, inputs: [], outputs: []

  @reward 5000
  
  @type t :: %__MODULE__{version: non_neg_integer, inputs: list(Input.t), outputs: list(Output.t)}
  @type encoded_tx :: binary
  @type tx_hash :: binary

  @spec sign(binary, t) :: t
  def sign(private_key, %__MODULE__{version: 1, inputs: inputs} = tx) do
    # Set all input signatures to 0 to create the message we're going to sign
    inputs2 = Enum.map(inputs, fn input -> %{input | signature: <<0 :: size(2048)>>} end)
    # Encode full tx, w/ zero'd signatures
    encoded_tx_without_signature = encode(%{tx | inputs: inputs2})
    # Use encoded tx as message for signature
    signature = :crypto.sign(:rsa, :rsa_digest_type, encoded_tx_without_signature, private_key)
    inputs3 = Enum.map(inputs2, fn input -> %{input | signature: signature} end)
    %{tx | inputs: inputs3}
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
    decoded_inputs = for <<input :: binary-size(97) <- inputs>>, do: Input.decode(input)
    decoded_outputs = for <<output :: binary-size(36) <- outputs>>, do: Output.decode(output)
    decode(rest, [%__MODULE__{inputs: decoded_inputs, outputs: decoded_outputs}|acc])
  end

  def decode(<<>>, acc) do
    acc
  end

  @spec coinbase :: t
  def coinbase do
    {_, fingerprint} = Base58Check.decode58check(Application.get_env(:wc, :coinbase_address))
    %__MODULE__{outputs: [%Output{fingerprint: fingerprint, value: @reward}]}
  end

  @spec is_coinbase(list(t)) :: true | false
  def is_coinbase(%__MODULE__{inputs: [], outputs: [%Output{size: @reward}]}) do
    true
  end

  def is_coinbase(_) do
    false
  end

end

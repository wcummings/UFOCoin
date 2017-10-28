defmodule OTC.Util do

  def binary_join([]) do
    <<>>
  end
  
  @spec binary_join(list(binary())) :: binary()
  def binary_join(bin_list) do
    Enum.reduce(bin_list, fn (bin, acc) -> acc <> bin end)
  end

  def difficulty_to_target(difficulty) do
    :math.pow(2, (256 - difficulty))
  end
  
end

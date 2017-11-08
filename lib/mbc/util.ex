defmodule MBC.Util do

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

  def ip_to_long({a, b, c, d}) do
    <<l :: size(32)>> = <<a, b, c, d>>
    l
  end

  def long_to_ip(<<a :: size(8), b :: size(8), c :: size(8), d :: size(8)>>) do
    {a, b, c, d}
  end

  def long_to_ip(l) do
    long_to_ip(<<l :: size(32)>>)
  end
  
end

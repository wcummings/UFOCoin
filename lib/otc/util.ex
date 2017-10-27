defmodule OTC.Util do

  @spec binary_join(list(binary())) :: binary()
  def binary_join(bin_list) do
    Enum.reduce(bin_list, fn (bin, acc) -> <<acc, bin>> end)
  end
  
end

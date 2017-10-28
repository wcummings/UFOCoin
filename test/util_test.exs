defmodule UtilTest do
  use ExUnit.Case
  doctest MBC

  test "binary_join" do
    assert <<0, 1, 2, 3>> == MBC.Util.binary_join([<<0>>, <<1>>, <<2>>, <<3>>])
  end

end

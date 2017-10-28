defmodule UtilTest do
  use ExUnit.Case
  doctest OTC

  test "binary_join" do
    assert <<0, 1, 2, 3>> == OTC.Util.binary_join([<<0>>, <<1>>, <<2>>, <<3>>])
  end

end

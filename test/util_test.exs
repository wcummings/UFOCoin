defmodule UtilTest do
  use ExUnit.Case
  doctest MBC

  test "binary_join" do
    assert <<0, 1, 2, 3>> == MBC.Util.binary_join([<<0>>, <<1>>, <<2>>, <<3>>])
  end

  test "ip_to_long" do
    assert MBC.Util.ip_to_long({172, 0, 0, 1}) == 2885681153
  end

  test "long_to_ip" do
    assert MBC.Util.long_to_ip(2885681153) == {172, 0, 0, 1}
  end

end

defmodule OTC.Mixfile do
  use Mix.Project

  def project do
    [app: :otc,
     version: "0.1.0",
     # elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [
      extra_applications: [:logger, :mnesia],
      mod: {OTC, []}
    ]
  end

  defp deps do
    [{:ranch, "~> 1.4.0"},
     {:msgpack, "~> 0.7.0"},
     {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
     {:distillery, "~> 1.5", runtime: false}]
  end
end

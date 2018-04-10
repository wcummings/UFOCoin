defmodule WC.Mixfile do
  use Mix.Project

  def project do
    [app: :wc,
     version: "0.1.0",
     # elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [
      extra_applications: [:logger, :mnesia, :crypto, :xmerl, :ranch],
      mod: {WC, []},
      applications: [:cachex, :nat, :base58check]
    ]
  end

  defp deps do
    [{:dialyxir, "~> 0.5", only: [:dev], runtime: false},
     {:distillery, "~> 1.5", runtime: false},
     {:nat, github: 'wcummings/erlang-nat'},
     {:cachex, "~> 2.1"},
     {:ranch, "~> 1.4.0"},
     {:base58check, github: "wcummings/base58check"},
     {:scribe, "~> 0.8"}]
  end
end

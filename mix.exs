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
      extra_applications: [:logger, :mnesia, :crypto],
      mod: {WC, []}
    ]
  end

  defp deps do
    [{:dialyxir, "~> 0.5", only: [:dev], runtime: false},
     {:distillery, "~> 1.5", runtime: false},
     {:nat_upnp, github: 'wcummings/nat_upnp'}]
  end
end

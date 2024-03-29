# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure for your application as:
#
#     config :otc, key: :value
#
# And access this configuration in your application as:
#
#     Application.get_env(:otc, :key)
#
# Or configure a 3rd-party app:
#
#     config :logger, level: :info
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"

config :wc,
  default_port: 9009,
  port: 9009,
  ip: :local, # {:nat, 9009} | {:ip, {192, 168, 0, 100}} | :local | :none
  # bind: {{192, 168, 0, 100}, 9009},
  # advertise: {{192, 168, 0, 100}, 9009},
  outbound_connections: 5,
  seed_dns: 'test1.wpc.io',
  mining_processes: 1,
  # data_dir: "/var/wc/"
  data_dir: "."

config :mnesia, dir: to_charlist File.cwd!  
  
config :logger, :console,
  format: "$time $metadata[$level] $levelpad$message\n",
  # metadata: [:module, :function, :line],
  metadata: [:block_hash],
  level: :info


FROM ubuntu
WORKDIR /tmp
RUN apt-get update && apt-get install -y wget make
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update && apt-get install -y elixir
RUN mkdir -p /tmp/build
COPY . build
WORKDIR /tmp/build
RUN cat ./config/config.exs
RUN mix local.hex --force
RUN mix clean
RUN mix compile
RUN MIX_ENV=prod mix release --env=prod
RUN mkdir -p /app
RUN cp _build/prod/rel/mbc/releases/0.1.0/mbc.tar.gz /app
WORKDIR /app/
RUN tar -xvzf mbc.tar.gz
CMD ["/app/bin/mbc", "foreground"]

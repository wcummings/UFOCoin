FROM ubuntu
RUN mkdir -p /app
WORKDIR /tmp
RUN apt-get update && apt-get install -y wget
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update && apt-get install -y elixir
COPY _build/dev/rel/* /app/
WORKDIR /app
CMD ["/app/bin/otc", "foreground"]

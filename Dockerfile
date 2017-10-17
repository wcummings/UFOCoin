FROM ubuntu
RUN apt-get update && apt-get install -y libssl1.0.0
RUN mkdir -p /app
COPY _build/prod/rel/otc/releases/0.1.0/otc.tar.gz /app
WORKDIR /app/
RUN tar -xvzf otc.tar.gz
CMD ["/app/bin/otc", "foreground"]

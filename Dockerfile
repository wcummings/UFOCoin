FROM ubuntu
RUN apt-get update && apt-get install -y libssl1.0.0
RUN mkdir -p /app
COPY _build/* /app/
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LC_ALL en_US.UTF-8
CMD ["/app/rel/otc/bin/otc", "foreground"]

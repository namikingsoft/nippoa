FROM debian:jessie

# set env
ARG TERM=linux
ARG DEBIAN_FRONTEND=noninteractive
ENV PATH $PATH:/root/.local/bin

# install stack
RUN apt-get update \
 && apt-get install --yes curl \
 && curl -sSL https://get.haskellstack.org/ | sh \
 && rm -rf /var/lib/apt/lists/*

# setup stack
COPY stack.yaml /app/
WORKDIR /app
RUN stack setup

# setup app
COPY . /app/
RUN stack build

# entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["idle"]

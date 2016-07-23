FROM debian:jessie

# set env
ARG TERM=linux
ARG DEBIAN_FRONTEND=noninteractive
ENV PATH /app/bin:/root/.local/bin:$PATH

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
RUN stack test

# entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["idle"]

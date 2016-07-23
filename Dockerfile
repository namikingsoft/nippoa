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

# entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["idle"]

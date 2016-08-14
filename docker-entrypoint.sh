#!/bin/sh -eu

# setup ghci
echo ':set prompt "\ESC[32m> \ESC[m"' > /root/.ghci

# setup timezone
if [ -n "${TIMEZONE-}" ]; then
  cp "/usr/share/zoneinfo/${TIMEZONE}" /etc/localtime
fi

# idle
if [ "$1" = "idle" ]; then
  exec tail -f /dev/null
fi

exec "$@"

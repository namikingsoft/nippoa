#!/bin/sh -eu

if [ "$1" = "idle" ]; then
  exec tail -f /dev/null
fi

exec "$@"

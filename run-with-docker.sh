#!/bin/sh

docker run --rm \
    --platform linux/amd64 \
    -v "$PWD:/work" \
    -w /work \
    ubuntu:24.04 \
    "$@"

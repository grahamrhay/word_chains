#!/usr/bin/bash

docker run --name word_chains -it --rm -v ${PWD}:/app -w /app -e REBAR_CACHE_DIR=/app erlang ./rebar3 compile

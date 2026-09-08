#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
# Use the JVM formatter: the native macOS launcher can crash in check mode.
exec scala run --scala 2.13.16 --dependency org.scalameta:scalafmt-cli_2.13:3.11.0 \
    --main-class org.scalafmt.cli.Cli --server=false -- "$@" project.scala src tests

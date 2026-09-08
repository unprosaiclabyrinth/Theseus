#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
./scripts/build.sh
classpath=$(cat target/runtime.classpath)
exec java -cp "$classpath" WorldApplication "$@"

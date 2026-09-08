#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
./scripts/check.sh --quiet
mkdir -p target
if ! mkdir target/.build-lock 2>/dev/null; then
    echo 'Another build is running. Retry when it finishes.' >&2
    exit 1
fi
trap 'rmdir target/.build-lock' EXIT HUP INT TERM
if [ "${1-}" = '--tests' ]; then
    scala compile project.scala src tests --server=false -d target --print-class-path > target/dependencies.classpath
else
    scala compile project.scala src --server=false -d target --print-class-path > target/dependencies.classpath
fi
classpath=$(cat target/dependencies.classpath)
javac --release 21 -cp "$classpath" -d target src/java/*.java
printf '%s\n' "$PWD/target:$classpath" > target/runtime.classpath

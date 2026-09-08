#!/bin/sh
set -eu
for tool in java javac scala; do
    command -v "$tool" >/dev/null || { echo "$tool is required (JDK 21+ and Scala CLI)." >&2; exit 1; }
done
java_major=$(java -XshowSettings:properties -version 2>&1 | sed -n 's/^[[:space:]]*java.specification.version = //p')
case "$java_major" in
    ''|*[!0-9]*) echo 'Could not identify a supported Java version.' >&2; exit 1 ;;
esac
if [ "$java_major" -lt 21 ]; then
    echo 'JDK 21 or newer is required.' >&2
    exit 1
fi
javac_major=$(javac -version 2>&1 | sed -n 's/^javac \([0-9][0-9]*\).*/\1/p')
case "$javac_major" in
    ''|*[!0-9]*) echo 'Could not identify a supported javac version.' >&2; exit 1 ;;
esac
if [ "$javac_major" -lt 21 ]; then
    echo 'javac 21 or newer is required; check that PATH points to a full JDK.' >&2
    exit 1
fi
# Scala 2's legacy runner does not support this Scala CLI command.
scala version >/dev/null
if [ "${1-}" != '--quiet' ]; then
    java --version
    javac --version
    scala version
fi

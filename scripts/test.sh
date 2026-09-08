#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
./scripts/build.sh --tests
classpath=$(cat target/runtime.classpath)
exec java -cp "$classpath" org.junit.runner.JUnitCore RegressionSuite PlannerSuite SeededPlannerSuite

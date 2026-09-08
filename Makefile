# Requires Scala CLI (the modern `scala` command) and JDK 21+.
.DEFAULT_GOAL := help
.PHONY: help all check build test lint format run sra mra uba rla rla-deterministic rla-biased rla-uniform lba tenk la-tenk clean

help all:
	@echo 'Targets: check build test run sra mra uba rla-deterministic rla-biased rla-uniform lba tenk la-tenk clean'
	@echo 'Custom options: ./scripts/run.sh --help'

check:
	./scripts/check.sh

build:
	./scripts/build.sh

test:
	./scripts/test.sh

format:
	./scripts/format.sh

lint:
	./scripts/format.sh --check
	python3 -m py_compile scripts/benchmark.py
	@for script in scripts/*.sh; do sh -n "$$script" || exit; done
	git diff --check

run uba:
	./scripts/run.sh --agent uba

sra:
	./scripts/run.sh --agent sra

mra:
	./scripts/run.sh --agent mra

rla rla-deterministic:
	./scripts/run.sh --agent rla -n 1

rla-biased:
	./scripts/run.sh --agent rla -n 0.8

rla-uniform:
	./scripts/run.sh --agent rla -n 0.3333333333333333

lba:
	./scripts/run.sh --agent lba

tenk:
	./scripts/run.sh --agent uba -t 10000 --quiet

la-tenk:
	./scripts/run.sh --agent rla --mixed -t 10000 --quiet

clean:
	rm -rf target .scala-build .bsp

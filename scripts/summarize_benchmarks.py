#!/usr/bin/env python3
"""Summarize completed experiments, optionally pairing scores with a reference CSV."""
import argparse
import csv
import json
import math
import statistics
from pathlib import Path


def paired_scores(rows, reference):
    def by_seed(data):
        result = {int(row['seed']): int(row['score']) for row in data}
        if len(result) != len(data):
            raise ValueError('Duplicate seeds make pairing ambiguous.')
        return result
    actual, baseline = by_seed(rows), by_seed(reference)
    common = sorted(actual.keys() & baseline.keys())
    if len(common) < 2:
        raise ValueError('At least two shared seeds are required for a paired comparison.')
    differences = [actual[seed]-baseline[seed] for seed in common]
    mean = statistics.mean(differences)
    margin = 1.96*statistics.stdev(differences)/math.sqrt(len(differences))
    return len(common), mean, mean-margin, mean+margin


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directories', nargs='+', type=Path)
    parser.add_argument('--reference', type=Path)
    args = parser.parse_args()
    reference = None
    if args.reference:
        with args.reference.open() as source:
            reference = list(csv.DictReader(source))
    print('# Planner benchmark results\n')
    print('Intervals are approximate 95% normal intervals. Timing includes JVM warmup and host load. '
          'Paired comparisons require identical worlds and episode limits; shared seeds alone do not establish this.\n')
    for directory in args.directories:
        report = json.loads((directory/'summary.json').read_text())
        print(f'## {directory.name}\n')
        print(f"Source: `{report['commit']}`; dirty: {report['dirty']}; seed: {report['seed']}; primitive step limit: {report['steps']}.\n")
        print('| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |')
        print('| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |')
        comparisons = []
        for label, result in report['experiments'].items():
            lo, hi = result['mean_score_normal_95_ci']
            print(f"| {label} | {result['trials']} | {result['mean_score']:.1f} [{lo:.1f}, {hi:.1f}] "
                  f"| {result['score_sd']:.1f} | {result['death_rate']:.1%} | {result['gold_rate']:.1%} "
                  f"| {result['mean_primitive_actions']:.1f} | {result['arrows_fired']} "
                  f"| {result['wumpus_kills']} | {result['planning_seconds']:.1f} |")
            if reference is not None:
                with (directory/f'{label}.txt.scores.csv').open() as source:
                    rows = list(csv.DictReader(source))
                comparisons.append((label, paired_scores(rows, reference)))
        print()
        if comparisons:
            print('| Configuration vs reference | Paired N | Mean score difference [95% CI] |')
            print('| --- | ---: | --- |')
            for label, (n, mean, lo, hi) in comparisons:
                print(f'| {label} | {n} | {mean:.1f} [{lo:.1f}, {hi:.1f}] |')
            print()


if __name__ == '__main__':
    main()

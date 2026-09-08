#!/usr/bin/env python3
"""Paired seeded planner experiments, using only the Python standard library."""
import argparse
import csv
import json
import itertools
import math
import platform
import shutil
import tempfile
import statistics
import subprocess
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def summary(rows):
    scores = [int(row['score']) for row in rows]
    n = len(scores)
    mean = statistics.mean(scores)
    sd = statistics.stdev(scores) if n > 1 else 0
    # Normal approximation, explicitly labelled. Prefer larger samples for inference.
    margin = 1.96 * sd / math.sqrt(n)
    return dict(trials=n, mean_score=mean, score_sd=sd,
                mean_score_normal_95_ci=[mean-margin, mean+margin],
                death_rate=statistics.mean(row['died'] == 'true' for row in rows),
                gold_rate=statistics.mean(row['gold_collected'] == 'true' for row in rows),
                mean_primitive_actions=statistics.mean(int(row['primitive_actions']) for row in rows),
                arrows_fired=sum(int(row['arrows_fired']) for row in rows),
                wumpus_kills=sum(int(row['wumpus_kills']) for row in rows),
                wumpus_kill_rate=statistics.mean(int(row['wumpus_kills']) > 0 for row in rows),
                planning_seconds=sum(int(row['planning_nanos']) for row in rows)/1e9)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--trials', type=int, default=30)
    parser.add_argument('--seed', type=int, default=42)
    parser.add_argument('--steps', type=int, default=50)
    parser.add_argument('--simulations', type=int, default=1000)
    parser.add_argument('--horizon', type=int, default=15)
    parser.add_argument('--discount', type=float, default=.2)
    parser.add_argument('--sweep', choices=['none', 'discount', 'simulations', 'horizon', 'shaping'], default='none')
    parser.add_argument('--case', choices=['all', 'A', 'B', 'C', 'D'], default='all')
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--skip-build', action='store_true')
    args = parser.parse_args()
    if args.trials < 2 or args.steps < 1:
        parser.error('Use at least two trials and one step.')
    args.output.mkdir(parents=True, exist_ok=False)
    if not args.skip_build:
        subprocess.run(['make', 'build'], cwd=ROOT, check=True)
    # Snapshot classes: a concurrent later build must not change this experiment.
    snapshot = tempfile.TemporaryDirectory(prefix='theseus-benchmark-')
    parts = (ROOT/'target/runtime.classpath').read_text().strip().split(':')
    copied = []
    for index, part in enumerate(parts):
        if Path(part).is_dir():
            destination = Path(snapshot.name)/str(index)
            shutil.copytree(part,destination)
            copied.append(str(destination))
        else:
            copied.append(part)
    classpath = ':'.join(copied)
    cases = {'A': ('canonical','uniform','none'),
             'B': ('canonical','informed','none'),
             'C': ('canonical','informed','potential'),
             'D': ('heuristic','informed','potential')}
    variants = [(args.simulations,args.discount)]
    if args.sweep == 'discount':
        variants = [(args.simulations,d) for d in [.2,.5,.8,.9,.95,.99]]
    elif args.sweep == 'simulations':
        variants = [(n,args.discount) for n in [100,250,500,1000,2500,5000]]
    horizons = [5,10,15,30] if args.sweep == 'horizon' else [args.horizon]
    shapings = ['none','legacy','potential'] if args.sweep == 'shaping' else [None]
    report = dict(commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
                  dirty=bool(subprocess.check_output(['git','status','--porcelain'],cwd=ROOT,text=True)),
                  platform=platform.platform(), java=subprocess.run(['java','--version'],capture_output=True,text=True,check=True).stdout,
                  seed=args.seed, steps=args.steps, horizon=args.horizon,
                  confidence_note='Normal approximation; small samples are exploratory. Timing includes JVM warmup; compare on the same host.',
                  experiments={})
    for horizon, shaping_override, (simulations, discount) in itertools.product(horizons, shapings, variants):
        for name, (tree, rollout, shaping) in cases.items():
            if args.case != 'all' and name != args.case:
                continue
            shaping = shaping_override or shaping
            label = f'{name}-n{simulations}-d{discount}-h{horizon}-s{shaping}'
            trace = (args.output/f'{label}.txt').resolve()
            command = ['java','-Xmx2g','-cp',classpath,'WorldApplication','--agent','uba',
                       '-t',str(args.trials),'-r',str(args.seed),'-s',str(args.steps),'--quiet',
                       '-f',str(trace),'--simulations',str(simulations),'--discount',str(discount),
                       '--horizon',str(horizon),'--tree-policy',tree,'--rollout',rollout,'--shaping',shaping]
            start = time.perf_counter()
            result = subprocess.run(command,cwd=ROOT,capture_output=True,text=True)
            (args.output/f'{label}.log').write_text(result.stdout+result.stderr)
            result.check_returncode()
            with Path(str(trace)+'.scores.csv').open() as source:
                rows = list(csv.DictReader(source))
            if len(rows) != args.trials:
                raise RuntimeError(f'{label}: incomplete trial output')
            report['experiments'][label] = dict(summary(rows), elapsed_seconds=time.perf_counter()-start,
                                               tree_policy=tree,rollout=rollout,shaping=shaping,
                                               simulations=simulations,discount=discount,horizon=horizon)
            (args.output/'summary.json').write_text(json.dumps(report,indent=2)+'\n')
            print(label, report['experiments'][label],flush=True)


if __name__ == '__main__':
    main()

# Remediation status

This work extends the previously delivered corrections rather than replacing them. Changes are split into reviewable commits on `fix/reliability-and-agent-correctness`.

## Preserved corrections

The earlier Rational normalization, terminal success/death rewards, final value-only action selection, agent resets, RLA/MRA corrections, LLM throttling and exact parsing, runtime agent selection, and declared source dependencies remain covered by regression tests. The historical simulator attribution, original reports and original score files remain intact.

## Added in this pass

| Area | Result | Evidence |
| --- | --- | --- |
| Planner structure | Separate model, generator, search tree, tree policies, rollouts, rewards and validated settings | `src/scala/planning/`, `docs/PLANNER.md` |
| Posterior correctness | Retain original Wumpus identity after a kill so equally likely worlds do not collapse | posterior mass and sampling tests |
| Observation correctness | Scream requires an actual newly fired arrow; it cannot recur on no-op or an empty shot | scream regression test |
| Model/simulator agreement | Wall handling, arrow exhaustion, orientation, shots, death, success and ordinary costs | 108 macro-action parity scenarios against Java |
| Search correctness | Explicit zero terminal continuation; visited means and visit-count tie breaks; zero-budgeted-edge fallback | terminal, UCT and tree-statistic tests |
| Tree reuse | Remove discarded ancestors and siblings while retaining the selected subtree | reachable-node and reset assertions |
| Experiments | Canonical/heuristic UCT, uniform/informed rollout, none/legacy/potential shaping, configurable budget/horizon/discount/exploration | CLI validation and benchmark harness |
| Metrics | Score, primitive actions, arrows, kills, death, gold and decision time per trial | Java parity and Python aggregation tests |
| Reproducibility | Independent seeded runs and isolated benchmark class snapshots; paired comparison by seed | ten-world regression, cross-JVM outcome checks, paired-analysis tests |
| Build compatibility | Scala and Java target Java 21 APIs/bytecode; reject an outdated javac | setup tests and class-file inspection |
| Maintenance | Pinned JVM Scalafmt; independently reported MUnit cases; CI lint and tests | `make lint`, `make test`, GitHub workflow |
| Documentation | Explicit macro costs, posterior assumptions, policy formulas, defaults, interpretation and limits | `docs/PLANNER.md` |

Validation passed locally: **31 MUnit tests and 4 Python tests**, including the 108 parity scenarios and two runs over ten fixed worlds. A clean rebuild passed during this pass. Java and Scala output use class-file version 65 (Java 21). Local execution used OpenJDK 22.0.1; the configured macOS/Linux JDK 21 CI workflow has not been run remotely here.

## Experimental interpretation

See `benchmarks/results/README.md` for measured configurations, raw per-trial data, paired comparisons, confidence intervals and provenance. The retained heuristic policy remains the default. None of these finite experiments establishes globally optimal parameters or a universal advantage of one tree-policy family.

Canonical UCT is tested with the same base exploration coefficient as the heuristic policy, but the latter multiplies it by substantial domain weights. The comparisons therefore measure these configurations, not equally tuned algorithms. Potential shaping and a finite search horizon can also influence results. Budget/horizon pilots use small samples and are explicitly exploratory.

Profile-guided changes remove repeated history lookups and temporary terminal-hazard sets. Root sampling uses cached indexed support, and generating a transition no longer recomputes it for each reward component. Exact belief support and decimal Monte Carlo values remain. Concurrent local experiments and JVM warmup prevent interpreting the timings as controlled speedup measurements.

## Deliberate limits and follow-up work

- The macro planner does not model the remaining primitive-step allowance. The simulator can truncate a queued macro at its episode limit; this is documented rather than hidden behind score claims.
- Potential shaping uses zero terminal potential, but finite rollout truncation leaves a boundary term. Policy invariance under this approximation is not promised.
- Agent implementations still use singleton state. Multiple simulations in one JVM are unsupported; separate processes and output paths are required.
- Live Gemini requests were not made. Offline tests cover response handling, timing and lifecycle. Actual model availability and credentials remain account-dependent.
- No new license was invented for inherited simulator code. The repository owner must establish redistribution terms.
- Bitset beliefs, approximate particles, primitive-double value storage, PUCT and progressive bias remain optional future experiments. They are not prerequisites for the tested fixes and were not substituted without evidence.

Historical Spring 2025 reports are not current benchmark results. The comparison reference for this pass is the previously delivered corrected version `028fc33`, not the original buggy planner. A historical-original rerun is not claimed.

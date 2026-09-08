# Local planner experiments

These are **exploratory local results**, not a claim of an optimal policy. There are 910 new trial runs over 100 shared world seeds, plus a 30-trial reference from the previously delivered corrected version. Configurations reuse seeds; these are not 940 independent worlds.

All runs use deterministic movement, fixed east-facing start, a 4×4 world and a 50-primitive-action episode limit. Seeds start at 42. The base exploration coefficient is sqrt(2). Trials record all outcomes; unsuccessful episodes are not discarded.

## What the measurements support

- At 1,000 simulations and discount .2, the retained heuristic configuration averaged **572.7** over 30 worlds. Canonical/uniform/no-shaping averaged 60.5; canonical/informed/no-shaping 124.3; canonical/informed/potential 215.2. These compare specific configurations, not equally tuned policy families.
- The previously delivered corrected version `028fc33` averaged **507.3** on those same 30 worlds. The new default's paired difference was **+65.4**, with an approximate 95% interval **[-66.6, 197.3]**. This does not establish an improvement over that baseline.
- Six discounts were evaluated on 100 worlds each at a **100-simulation budget**. Mean scores for .2/.5/.8/.9/.95/.99 were **311.9/339.8/338.6/303.2/272.8/124.7**. Higher discounts increased risk: death rates were 3%/0%/7%/20%/19%/29%. The .5 and .8 mean differences versus .2 remain uncertain. The .99 paired interval was entirely negative before multiple-comparison adjustment. No default change is justified by these results.
- The budget pilot covered 100, 250, 500, 1,000, 2,500 and 5,000 simulations on ten worlds each. More simulations did not monotonically improve every result; the small sample cannot select a reliable optimum.
- The shaping pilot used canonical/informed search, 100 simulations and 30 worlds: none averaged 152.7, terminal-safe legacy 345.8, and potential 110.1. Potential shaping is not guaranteed to improve a finite search. These pilot results warrant follow-up, not a silent default substitution.
- Horizons 5, 10, 15 and 30 were evaluated on ten worlds each. Their wide intervals do not support an optimum.

The defaults remain 1,000 simulations, horizon 15, discount .2, heuristic UCT, informed rollout and potential shaping. This preserves the previously corrected configuration while making alternatives measurable. A setting selected from these experiments should be confirmed on held-out seeds before changing the default.

## Data and comparisons

| Study | Trials per setting | Data | Summary |
| --- | ---: | --- | --- |
| Four policies | 30 | [ablation metadata](ablation/summary.json) and adjacent CSVs | [paired baseline comparison](ablation-comparison.md) |
| Six discounts | 100 | [discount metadata](discounts/summary.json) and adjacent CSVs | [paired discount comparison](discount-comparison.md) |
| Six budgets | 10 | [budget metadata](budgets/summary.json) and adjacent CSVs | [pilot summaries](pilots.md) |
| Three shaping strategies | 30 | [shaping metadata](shaping/summary.json) and adjacent CSVs | [pilot summaries](pilots.md) |
| Four horizons | 10 | [horizon metadata](horizons/summary.json) and adjacent CSVs | [pilot summaries](pilots.md) |
| Previous corrected version | 30 | [configuration](previous-corrected-baseline/config.json), [scores](previous-corrected-baseline/scores.csv) | [paired baseline comparison](ablation-comparison.md) |

The older reference predates per-trial outcome metrics; its death/gold rates are not inferred from score. It is the corrected `028fc33` baseline, not the original Spring 2025 planner.

## Provenance and limits

Each study retains its recorded source revision, dirty flag and runtime details. Later runs also record the class-snapshot hash. The initial ablation recorded a dirty tree, so it should be treated as a local exploratory result rather than a clean-release benchmark. Subsequent code refinements preserved all deterministic outcome fields across ten independently rerun worlds at budgets 100 and 1,000. Source formatting and test restructuring were separate commits.

Intervals are approximate 95% normal intervals for mean scores or paired score differences; they are unadjusted for testing multiple configurations. Budget and horizon pilots are deliberately small, and even 100-seed estimates remain broad. Do not interpret overlapping or marginal intervals as a ranking guarantee.

Several jobs ran concurrently. Planning times include JVM warmup, scheduler contention and the agent adapter. They are useful recorded costs, **not controlled speedup measurements**. Re-run serially on an idle host to compare performance.

A JFR profile of the corrected reference contained 1,084 sampled stacks with `History.lastOption` as the innermost UBA frame and 679 with terminal-state predicates. This supported avoiding repeated history queries and temporary hazard sets. Exact support was retained; decimal values were not replaced speculatively.

See the [planner guide](../../docs/PLANNER.md) for reproduction commands, macro-action semantics and the remaining primitive-step cutoff limitation.

# Utility-based planning

Theseus uses POMCP-style Monte Carlo tree search with root sampling from an exact finite belief support, deterministic generative simulation, macro-actions, configurable rollout policies, and optional reward and tree-policy biases. Canonical UCT is available as an experiment; it does not turn the complete agent into textbook POMCP.

## Components

| File in `src/scala/planning/` | Responsibility |
| --- | --- |
| `UbaModel.scala` | World hypotheses, states, exact observations, orientations and macro-actions |
| `GenerativeModel.scala` | One transition, its observation, and its reward |
| `RewardModel.scala` | Simulator score and separately selected shaping |
| `SearchTree.scala` | Alternating action/history nodes, statistics, expansion and subtree reuse |
| `TreePolicies.scala` | Canonical UCT, named domain biases, and final mean-value selection |
| `RolloutPolicies.scala` | Uniform legal actions or informed rollouts |
| `POMCP.scala` | Root sampling, simulation and backup |
| `PlannerConfig.scala` | Validated settings and CLI parsing |

`UtilityBasedAgent.scala` adapts the planner's moves into a primitive-action queue. Glitter takes priority and immediately grabs gold.

## Model and terminal semantics

Coordinates are `(x,y)`, from `(1,1)` to `(4,4)`. Java's grid is indexed `[y-1][x-1]`. The prior has 25,200 worlds: 105 distinct unordered pit pairs, 15 Wumpus locations, and 16 gold locations. Pits and the live Wumpus cannot start on the agent. Gold, pits and Wumpus may otherwise overlap, matching world generation.

Every surviving hypothesis has equal posterior probability because the prior is uniform and dynamics and observations are deterministic. After a kill, hypotheses retain their original Wumpus location as latent identity. Dropping that identity would merge distinct worlds in a set and corrupt posterior weights. A cached vector permits uniform indexed root sampling without shuffling the support.

This support representation is **not valid for stochastic movement or noisy observations**. UBA therefore requires `-n 1`. RLA uses a separate weighted belief representation.

Entering a pit or live Wumpus pays the death penalty once. Successful `Grab` pays the success reward once and transitions to `Won`. Both have zero continuation return. Killing the Wumpus, losing an arrow, failing to grab, and doing nothing are nonterminal.

The observation contains stench, breeze, glitter and scream. Scream denotes the transition from a live to a dead Wumpus, not every subsequent state without a Wumpus. The planner excludes wall-bound movement from search; the model also implements wall bumps correctly for direct calls.

## Macro-actions and scoring

| Planner move | Primitive sequence | Ordinary score |
| --- | --- | ---: |
| `GoForward` | forward | -1 |
| `GoLeft` | left, forward | -2 |
| `GoRight` | right, forward | -2 |
| `GoBack` | two identical turns, forward | -3 |
| `Shoot` | shoot | -10 |
| `ShootLeft` | left, shoot | -11 |
| `ShootRight` | right, shoot | -11 |
| `Grab` | grab | +1000 on success, otherwise -1 |
| `NoOp` | no-op | 0 |

A lethal forward step costs -1000 instead of -1; preceding turns still cost -1 each. Shooting without an arrow costs -1, plus any preceding turn, and cannot kill. Search excludes shots when the arrow is gone. A wall bump still pays the movement cost.

Discount and horizon are measured in **macro-actions**, not primitive actions. A reward received on the first macro has weight 1; the next has weight `discount`, and so on. The simulator's `-s` limit counts primitive actions and can stop partway through a queued macro. Search does not model the remaining simulator step allowance, so near that external cutoff its forecast can extend beyond the episode. Use identical step limits in comparisons; this remains a known modeling limitation.

## Search policies and values

History nodes expand into action nodes; action nodes expand into observation/history nodes. Visits increment once per traversed node. Means use the new visit count in `oldMean + (return-oldMean)/visits`. The first root simulation expands the root and rolls out, so a fresh root with N simulations has N-1 traversed action edges. A budget of one safely returns `NoOp` when no action has been evaluated.

Canonical selection first samples unvisited actions, including shots, then uses:

```text
mean + exploration * sqrt(log(parentVisits) / actionVisits)
```

Heuristic selection preserves the historical preference for unvisited non-shoot actions. Afterwards its exploration bonus uses `visits+1` and named weights: forward 111, left/right 110, back 1, shots 50 times historical stench count, no-op 50, grab 0. These are domain preferences, not canonical UCT. Unvisited means zero visits, not absence of observation children: terminal action edges may never have such children.

The actual executed action uses maximum visited mean, then visit count, then stable move order. It never receives an exploration bonus. Pruning retains only the selected subtree and clears its obsolete parent; tree reuse remains enabled.

Uniform rollout samples all legal moves. Informed rollout grabs on glitter, prefers available shots on stench, and otherwise samples forward/left/right/no-op. Both sample through the seeded Scala RNG.

## Settings

| CLI option | Default | Allowed values |
| --- | --- | --- |
| `--simulations` | 1000 | positive integer |
| `--horizon` | 15 | integer 1–100 |
| `--discount` | 0.2 | finite number 0–1 |
| `--exploration` | sqrt(2) | finite nonnegative number |
| `--tree-policy` | heuristic | canonical, heuristic |
| `--rollout` | informed | uniform, informed |
| `--shaping` | potential | none, legacy, potential |

These options require `--agent uba`. Defaults retain the previously corrected policy settings; they are not claimed to be empirically optimal. In particular, 0.2 makes distant returns very small. Change it through an experiment rather than an undocumented source edit.

Environment reward is always the simulator score. Shaping affects planning only:

- `none`: zero added reward.
- `potential`: `discount * Phi(next) - Phi(current)`.
- `legacy`: `Phi(next) - Phi(current)`, retaining the old nonterminal distance/kill preference with explicit zero terminal potential.

`Phi` is minus four times Manhattan distance to gold, plus nine after a Wumpus kill; terminal potential is zero. The legacy option is a terminal-safe adaptation, not an exact replay of the buggy original planner. Potential-based shaping's usual policy-invariance argument does not establish equivalence under a truncated rollout horizon: the residual boundary potential can affect finite-search decisions. No such equivalence is claimed here.

## Experiments

Build once, then run paired seeded worlds:

```sh
python3 scripts/benchmark.py --trials 100 --output /tmp/theseus-ablation
python3 scripts/benchmark.py --case D --sweep discount --trials 100 --output /tmp/theseus-discounts
python3 scripts/benchmark.py --case D --sweep simulations --trials 100 --output /tmp/theseus-budgets
python3 scripts/benchmark.py --case C --sweep shaping --trials 100 --output /tmp/theseus-shaping
python3 scripts/benchmark.py --case D --sweep horizon --trials 100 --output /tmp/theseus-horizons
```

The output directory must be new, preventing accidental replacement of experiments. `--skip-build` uses an existing build. Classes are copied into a temporary snapshot so later builds cannot alter a running experiment. The script uses argument arrays, never shell interpolation.

Cases: A = canonical/uniform/no shaping; B = canonical/informed/no shaping; C = canonical/informed/potential; D = heuristic/informed/potential. Discount sweep: .2, .5, .8, .9, .95, .99. Budget sweep: 100, 250, 500, 1000, 2500, 5000. Horizon sweep: 5, 10, 15, 30.

CSV records seed, score, primitive actions (including no-op), arrows fired, Wumpus kills, death, gold retrieval and nanoseconds spent choosing actions. `END_TRIAL` is not a primitive action. Choosing includes the adapter and queued actions, not just tree search. Timing is nondeterministic and includes JVM warmup; avoid concurrent workloads for performance measurements. JSON records configuration, source revision, dirty status, host/runtime, standard deviation and an approximate 95% normal confidence interval for the mean. Small samples are exploratory. Compare paired seeds, inspect uncertainty, and confirm any proposed setting on held-out seeds.

`make test` includes deterministic model/simulator parity, tree-statistic tests, posterior sampling tests, and two identical runs over ten seeded worlds. These are correctness checks, not claims that every seeded world is solvable or that one policy dominates.

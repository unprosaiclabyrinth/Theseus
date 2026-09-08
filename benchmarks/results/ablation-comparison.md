# Planner benchmark results

Intervals are approximate 95% normal intervals. Timing includes JVM warmup and host load. Paired comparisons require identical worlds and episode limits; shared seeds alone do not establish this.

## ablation

Source: `1d29b3fb2540c5a15ae3567ba9781bedbc113add`; dirty: True; seed: 42; primitive step limit: 50.

| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A-n1000-d0.2-h15-snone | 30 | 60.5 [-30.9, 151.9] | 255.4 | 0.0% | 6.7% | 46.7 | 7 | 4 | 187.9 |
| B-n1000-d0.2-h15-snone | 30 | 124.3 [-0.2, 248.7] | 347.7 | 0.0% | 13.3% | 43.8 | 8 | 6 | 200.5 |
| C-n1000-d0.2-h15-spotential | 30 | 215.2 [59.3, 371.2] | 435.7 | 0.0% | 23.3% | 42.7 | 7 | 5 | 127.1 |
| D-n1000-d0.2-h15-spotential | 30 | 572.7 [389.5, 755.9] | 511.9 | 0.0% | 60.0% | 30.5 | 9 | 2 | 149.2 |

| Configuration vs reference | Paired N | Mean score difference [95% CI] |
| --- | ---: | --- |
| A-n1000-d0.2-h15-snone | 30 | -446.8 [-632.1, -261.5] |
| B-n1000-d0.2-h15-snone | 30 | -383.1 [-564.2, -201.9] |
| C-n1000-d0.2-h15-spotential | 30 | -292.1 [-461.2, -123.0] |
| D-n1000-d0.2-h15-spotential | 30 | 65.4 [-66.6, 197.3] |


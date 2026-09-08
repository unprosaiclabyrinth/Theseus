# Planner benchmark results

Intervals are approximate 95% normal intervals. Timing includes JVM warmup and host load. Paired comparisons require identical worlds and episode limits; shared seeds alone do not establish this.

## budgets

Source: `9f7ee2481799cc2dcf54f3e689b2b66a4ef1dc1d`; dirty: False; seed: 42; primitive step limit: 50.

| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| D-n100-d0.2-h15-spotential | 10 | 269.7 [-39.8, 579.2] | 499.4 | 0.0% | 30.0% | 36.6 | 1 | 0 | 24.3 |
| D-n250-d0.2-h15-spotential | 10 | 77.6 [-382.9, 538.1] | 742.9 | 20.0% | 30.0% | 31.2 | 0 | 0 | 40.2 |
| D-n500-d0.2-h15-spotential | 10 | 369.4 [39.3, 699.5] | 532.7 | 0.0% | 40.0% | 34.9 | 4 | 0 | 42.6 |
| D-n1000-d0.2-h15-spotential | 10 | 372.3 [40.9, 703.7] | 534.7 | 0.0% | 40.0% | 32.4 | 3 | 0 | 53.7 |
| D-n2500-d0.2-h15-spotential | 10 | 476.0 [138.3, 813.7] | 544.8 | 0.0% | 50.0% | 28.3 | 3 | 0 | 109.1 |
| D-n5000-d0.2-h15-spotential | 10 | 478.6 [142.5, 814.7] | 542.3 | 0.0% | 50.0% | 28.6 | 3 | 0 | 205.4 |

## shaping

Source: `080f966f4f39ee99afe0c57a56fb2b4ba2aab1c7`; dirty: False; seed: 42; primitive step limit: 50.

| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| C-n100-d0.2-h15-snone | 30 | 152.7 [16.1, 289.3] | 381.8 | 0.0% | 16.7% | 43.7 | 8 | 6 | 155.4 |
| C-n100-d0.2-h15-slegacy | 30 | 345.8 [168.4, 523.2] | 495.6 | 0.0% | 36.7% | 39.7 | 11 | 6 | 111.2 |
| C-n100-d0.2-h15-spotential | 30 | 110.1 [-16.6, 236.7] | 353.9 | 0.0% | 13.3% | 44.9 | 8 | 5 | 98.2 |

## horizons

Source: `15152740b0aa29e7beb3b9a9448c80db45d057b5`; dirty: True; seed: 42; primitive step limit: 50.

| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| D-n100-d0.2-h5-spotential | 10 | 267.9 [-42.4, 578.2] | 500.6 | 0.0% | 30.0% | 36.5 | 2 | 1 | 22.6 |
| D-n100-d0.2-h10-spotential | 10 | 368.4 [39.7, 697.1] | 530.3 | 0.0% | 40.0% | 36.7 | 2 | 2 | 29.7 |
| D-n100-d0.2-h15-spotential | 10 | 269.7 [-39.8, 579.2] | 499.4 | 0.0% | 30.0% | 36.6 | 1 | 0 | 23.8 |
| D-n100-d0.2-h30-spotential | 10 | 274.3 [-148.7, 697.3] | 682.4 | 10.0% | 40.0% | 30.6 | 2 | 0 | 16.3 |


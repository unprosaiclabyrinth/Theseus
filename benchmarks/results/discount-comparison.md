# Planner benchmark results

Intervals are approximate 95% normal intervals. Timing includes JVM warmup and host load. Paired comparisons require identical worlds and episode limits; shared seeds alone do not establish this.

## discounts

Source: `c2ef6ac7374c12519cbb1afec570ca22bd3478f8`; dirty: False; seed: 42; primitive step limit: 50.

| Configuration | N | Mean score [95% CI] | SD | Death | Gold | Mean actions | Arrows | Kills | Planning seconds |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| D-n100-d0.2-h15-spotential | 100 | 311.9 [205.4, 418.5] | 543.6 | 3.0% | 37.0% | 37.4 | 23 | 5 | 289.6 |
| D-n100-d0.5-h15-spotential | 100 | 339.8 [242.4, 437.2] | 497.0 | 0.0% | 37.0% | 37.0 | 50 | 17 | 278.8 |
| D-n100-d0.8-h15-spotential | 100 | 338.6 [216.7, 460.6] | 622.3 | 7.0% | 44.0% | 32.2 | 80 | 22 | 170.0 |
| D-n100-d0.9-h15-spotential | 100 | 303.2 [147.9, 458.5] | 792.3 | 20.0% | 53.0% | 25.2 | 78 | 17 | 183.0 |
| D-n100-d0.95-h15-spotential | 100 | 272.8 [121.5, 424.1] | 771.9 | 19.0% | 49.0% | 25.7 | 68 | 23 | 154.1 |
| D-n100-d0.99-h15-spotential | 100 | 124.7 [-40.9, 290.3] | 845.0 | 29.0% | 44.0% | 23.0 | 75 | 14 | 133.2 |

| Configuration vs reference | Paired N | Mean score difference [95% CI] |
| --- | ---: | --- |
| D-n100-d0.2-h15-spotential | 100 | 0.0 [0.0, 0.0] |
| D-n100-d0.5-h15-spotential | 100 | 27.9 [-78.8, 134.6] |
| D-n100-d0.8-h15-spotential | 100 | 26.7 [-96.6, 150.1] |
| D-n100-d0.9-h15-spotential | 100 | -8.7 [-156.7, 139.2] |
| D-n100-d0.95-h15-spotential | 100 | -39.1 [-168.4, 90.2] |
| D-n100-d0.99-h15-spotential | 100 | -187.2 [-338.0, -36.4] |


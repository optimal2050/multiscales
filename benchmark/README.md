# Benchmarks

Not tests. These measure time and memory on data large enough that the
difference between "runs" and "runs on your laptop" is visible, and they are
excluded from the build (`^benchmark$` in `.Rbuildignore`).

## Running

```r
Sys.setenv(MULTISCALES_BENCH_SCALE = "small")   # small | medium | large
source("benchmark/gen-data.R")                  # writes benchmark/_data/
source("benchmark/bench-recast.R")              # writes benchmark/results/
```

`small` is a few hundred thousand rows and runs in seconds; `large` is the
flagship shape (8760 timeslices x 1477 regions x 2 years) and is the one worth
quoting. `benchmark/_data/` and `benchmark/results/` are gitignored except for
committed result CSVs you choose to keep.

## What is measured

Three memory numbers, because no single one tells the story:

| column | source | what it catches |
|---|---|---|
| `r_heap_mb` | `gc()` max used | R-side intermediates -- the old `keysets` scan and the identifier cross join both showed up here |
| `arrow_mb` | `arrow::default_memory_pool()$max_memory` delta | work pushed into the Arrow C++ engine |
| `rss_mb` | `ps::ps_memory_info()` peak, when `ps` is installed | what the operating system actually reserved |

plus `sec` from `system.time()`.

## Reading a result

Every case also recomputes a conservation check, so a configuration that is
fast because it is *wrong* fails here rather than looking like a win. The
`ok` column must be `TRUE` in every row; treat a `FALSE` as a bug report, not
a benchmark result.

Compare two runs with `benchmark/compare.R`, which diffs on the case key and
reports ratios, so a pull request can show before/after instead of describing
it.

## The cases

- `recast_scale` aggregating each axis, lazy and collected
- `recast_product` converting both axes in one call
- `join_scale`, which should be zero-scan on a lazy input
- `diagnostics` on vs off, which is the cost of the data-scanning checks
- `batch` off vs on, which trades peak memory for wall time
- `missing_sources` `"na"` vs `"ignore"`, the cost of the partial-source repair

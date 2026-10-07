
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discretescales

<!-- badges: start -->

<!-- badges: end -->

Nested discrete scales for optimization and simulation models, without
naming the dimension. A **scale** is a flat table of *atoms* plus the
ordered *frames* that group them — and that shape fits time, space,
industries, income brackets, temperature regimes or technology vintages
equally well.

`discretescales` is the engine under
[timescales](https://github.com/optimal2050/timescales) (calendars) and
[geoscales](https://github.com/optimal2050/geoscales) (regions), whose
classes are subclasses of `DiscreteScale`. Use it directly for any other
dimension.

## Installation

``` r
pak::pkg_install("optimal2050/discretescales")
```

## A scale in one call

Declare the hierarchy, coarsest frame first:

``` r
library(discretescales)

industries <- data.frame(
  section  = c("C",   "C",   "C",   "D",   "D"),
  division = c("C10", "C10", "C11", "D35", "D35"),
  class    = c("C101", "C102", "C110", "D351", "D352"),
  gva      = c(120, 80, 45, 300, 150)
)

ind <- scale_from_leaftable(
  industries,
  frames = c("section", "division", "class"),
  name   = "nace"
)

ind
#> DiscreteScale: nace
#> Frames (3, coarsest first):
#>   - section (2)
#>     - division (3)
#>       - class (5)
#> Atoms: 5
#> Weights: gva (default: gva)
```

Every structural question is answered from the atoms, never from an
assumed tree:

``` r
scale_frames(ind)
#> [1] "section"  "division" "class"
scale_units(ind, "division")
#> [1] "C10" "C11" "D35"
scale_nests(ind, "section", "division")
#> [1] TRUE
```

Hierarchies that cross-cut are normal and supported — one detailed code
may belong to several aggregates. `scale_nests()` tells you whether a
given pair happens to nest; conversion never depends on it.

``` r
summary(ind)
#> <summary of DiscreteScale 'nace'>
#>   frames:        section (2) / division (3) / class (5)
#>   atoms:          5
#>   weight totals:  gva = 695  (default: gva)
#>   nesting:        section > division: nested
#>   nesting:        division > class: nested
```

## Design

- **Atoms are the ground truth.** Frames are partitions of the atoms,
  not necessarily a strict tree; conversion always routes through the
  atom layer.
- **Weights are named and plural.** A scale can carry `gva`,
  `employment` and `count` side by side, with one declared default.
- **Partial coverage is first class.** An atom with no code at a frame
  is `NA`, not an error, and a subset records what fraction of its
  parent it keeps.
- **Backend-agnostic.** The verbs are written on dplyr only, so the same
  pipeline runs on a `data.frame`, a `data.table`, or an arrow dataset;
  lazy inputs stay lazy.

## License

Apache License 2.0.

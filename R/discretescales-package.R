#' discretescales: Nested Scales and Multi-Dimensional Indexes for Modeling
#'
#' A dimension-agnostic representation of nested discrete scales: a flat table
#' of atoms plus the ordered frames that group them, conversion of data between
#' resolutions, combination of several scales into a product index, and
#' construction of new frames by clustering.
#'
#' Time and space live in dimension packages built on this one:
#' [timescales](https://github.com/optimal2050/timescales)' `Calendar` and
#' [geoscales](https://github.com/optimal2050/geoscales)' `Geoscale` are
#' subclasses of `DiscreteScale`, so every verb here accepts them. Any other
#' dimension -- industries, income brackets, temperature regimes, technology
#' vintages -- is declared directly with [`scale_from_leaftable()`], or given
#' a package of its own the same way: see
#' `vignette("dimension-package", package = "discretescales")`.
#'
#' A clustering is treated as scale construction: [`cluster_scale()`] returns
#' the scale with a new coarser frame inserted above the one clustered, so
#' recasting to clusters, within-cluster shares, coverage and products all work
#' with no special casing. [`scale_distance()`] and its registry supply the
#' metrics, [`cluster_contiguous()`] the order- and adjacency-constrained
#' variants, and [`cluster_sweep()`] reports quality across `k` without
#' choosing one for you.
#'
#' @keywords internal
#' @importFrom rlang .data :=
"_PACKAGE"

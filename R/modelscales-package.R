#' modelscales: Nested Scales and Multi-Dimensional Indexes for Modeling
#'
#' A dimension-agnostic representation of nested discrete scales: a flat table
#' of atoms plus the ordered frames that group them, conversion of data between
#' resolutions, combination of several scales into a product index, and
#' construction of new frames by clustering.
#'
#' Time and space are provided by the companion packages
#' [timescales](https://github.com/optimal2050/timescales) and
#' [geoscales](https://github.com/optimal2050/geoscales). Any other dimension
#' -- industries, income brackets, temperature regimes, technology vintages --
#' is declared directly with [`scale_from_leaftable()`].
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

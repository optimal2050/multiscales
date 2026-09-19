#' multiscales: Nested Scales and Multi-Dimensional Indexes for Modeling
#'
#' A dimension-agnostic representation of nested discrete scales: a flat table
#' of atoms plus the ordered frames that group them, conversion of data between
#' resolutions, and combination of several scales into a product index.
#'
#' Time and space are provided by the companion packages
#' [timescales](https://github.com/optimal2050/timescales) and
#' [geoscales](https://github.com/optimal2050/geoscales), whose `Calendar` and
#' `Geoscale` classes are subclasses of [`Scale`]. Any other dimension --
#' industries, income brackets, temperature regimes, technology vintages -- is
#' declared directly with [`scale_from_leaftable()`].
#'
#' @keywords internal
#' @importFrom rlang .data :=
"_PACKAGE"

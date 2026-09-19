# =========================================================================== #
# Measurement helpers.
#
# Three memory numbers, because each misses something the others catch: R heap
# misses work pushed into Arrow, the Arrow pool misses R-side intermediates,
# and RSS catches both but is noisy and unavailable without `ps`.
# =========================================================================== #

.mb <- function(bytes) round(bytes / 1024^2, 1)

.rss_mb <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) return(NA_real_)
  mi <- ps::ps_memory_info()
  nm <- intersect(c("peak_wset", "rss"), names(mi))
  if (length(nm) == 0L) return(NA_real_)
  .mb(mi[[nm[[1L]]]])
}

.arrow_pool_bytes <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) return(NA_real_)
  p <- arrow::default_memory_pool()
  # `max_memory` is a field, and it is monotone for the session -- deltas
  # between cases are what mean anything.
  tryCatch(p$max_memory, error = function(e) NA_real_)
}

#' Time and measure one expression
#'
#' @param label Case name.
#' @param expr Expression to run.
#' @param check A function applied to the result returning TRUE when the
#'   numbers are right. A fast wrong answer is not a benchmark result.
bench_case <- function(label, expr, check = function(x) NA) {
  gc(reset = TRUE, full = TRUE)
  a0 <- .arrow_pool_bytes()
  t <- system.time(res <- force(expr))
  g <- gc()
  a1 <- .arrow_pool_bytes()

  ok <- tryCatch(isTRUE(check(res)), error = function(e) FALSE)
  # gc() reports cells and megabytes in alternating columns; the last one is
  # "max used" in Mb, and both rows (Ncells, Vcells) count toward the peak.
  heap_mb <- sum(g[, ncol(g)])
  data.frame(
    case      = label,
    sec       = round(t[["elapsed"]], 3),
    r_heap_mb = round(heap_mb, 1),
    arrow_mb  = if (is.na(a0) || is.na(a1)) NA_real_ else .mb(a1 - a0),
    rss_mb    = .rss_mb(),
    ok        = ok,
    stringsAsFactors = FALSE
  )
}

#' Row count without materialising, where the backend can answer cheaply
bench_nrow <- function(x) {
  tryCatch(nrow(x), error = function(e) NA_integer_)
}

bench_write_results <- function(tab, scale_name) {
  dir.create(file.path("benchmark", "results"), recursive = TRUE,
             showWarnings = FALSE)
  sha <- tryCatch(
    substr(system2("git", c("rev-parse", "--short", "HEAD"),
                   stdout = TRUE, stderr = FALSE)[1], 1, 8),
    error = function(e) "nogit", warning = function(e) "nogit")
  if (length(sha) != 1L || is.na(sha) || !nzchar(sha)) sha <- "nogit"
  f <- file.path("benchmark", "results",
                 sprintf("%s-%s-%s.csv", Sys.Date(), sha, scale_name))
  utils::write.csv(tab, f, row.names = FALSE)
  message("wrote ", f)
  invisible(f)
}

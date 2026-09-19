# =========================================================================== #
# Test-tier resolution -- the scales family's slim ladder.
#
# Tiers (check < fast < full), selected via SCALES_TEST_TIER (shared with
# timescales and geoscales so one setting drives every suite):
#   check - what R CMD check runs: core invariants, data.frame backend
#   fast  - default for local devtools::test(): + data.table + lazy smoke
#   full  - the whole backend sweep (dtplyr + arrow on every entry point)
#           and the full property grid
#
# When SCALES_TEST_TIER is unset the tier defaults to "check" under
# R CMD check (detected via _R_CHECK_PACKAGE_NAME_ / NOT_CRAN=false) and to
# "fast" everywhere else. An invalid value is an error, never a silent
# fallback.
# =========================================================================== #

.tier_levels <- c(check = 0L, fast = 1L, full = 2L)

scales_test_tier <- function() {
  t <- tolower(Sys.getenv("SCALES_TEST_TIER", ""))
  if (nzchar(t)) {
    if (!t %in% names(.tier_levels)) {
      stop("SCALES_TEST_TIER must be one of: ",
           paste(names(.tier_levels), collapse = ", "), " (got '", t, "')")
    }
    return(t)
  }
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
      identical(Sys.getenv("NOT_CRAN"), "false")) {
    return("check")
  }
  "fast"
}

skip_if_tier_below <- function(tier) {
  stopifnot(tier %in% names(.tier_levels))
  if (.tier_levels[[scales_test_tier()]] < .tier_levels[[tier]]) {
    testthat::skip(paste0("tier '", scales_test_tier(), "' < required '",
                          tier, "' (set SCALES_TEST_TIER=", tier,
                          " to run)"))
  }
}

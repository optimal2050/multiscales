# =============================================================================
# Internal utilities
# =============================================================================

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Blank-or-NA to NA
#'
#' Real-world scale tables use `""` and `NA` interchangeably for "this atom has
#' no code at this frame" (a rest-of-world row, an unclassified industry).
#' Normalise both to `NA_character_` on the way in.
#' @noRd
.blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & trimws(x) == ""] <- NA_character_
  x
}

#' Stop with a formatted message and no call context
#' @noRd
.stop <- function(...) stop(sprintf(...), call. = FALSE)

#' Warn with a formatted message and no call context
#' @noRd
.warn <- function(...) warning(sprintf(...), call. = FALSE)

#' Name of a Scale (meta$name), required for conversion and attach
#'
#' The crosswalk's label columns and `join_scale()`'s attached columns are
#' named after the object, so those operations need a non-empty name.
#' @noRd
.scale_name <- function(x, require = TRUE, arg = "x") {
  nm <- S7::prop(x, "meta")$name %||% ""
  if (require && (!is.character(nm) || length(nm) != 1L || is.na(nm) ||
                  !nzchar(nm))) {
    .stop(paste0("`%s` has no name; conversion and attach need a named ",
                 "scale -- set meta$name, or pass `name=` to ",
                 "scale_from_leaftable()"), arg)
  }
  nm
}

#' Truncated comma-separated preview of a vector
#' @noRd
.preview <- function(x, n = 3L) {
  x <- as.character(x)
  if (length(x) <= n) return(paste(x, collapse = ", "))
  sprintf("%s, ... (%d total)", paste(utils::head(x, n), collapse = ", "),
          length(x))
}
